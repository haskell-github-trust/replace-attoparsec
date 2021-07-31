-- |
-- Module    : Replace.Attoparsec.Text
-- Copyright : ©2019 James Brock
-- License   : BSD2
-- Maintainer: James Brock <jamesbrock@gmail.com>
--
-- __Replace.Attoparsec__ is for finding text patterns, and also
-- replacing or splitting on the found patterns.
-- This activity is traditionally done with regular expressions,
-- but __Replace.Attoparsec__ uses "Data.Attoparsec" parsers instead for
-- the pattern matching.
--
-- __Replace.Attoparsec__ can be used in the same sort of “pattern capture”
-- or “find all” situations in which one would use Python
-- <https://docs.python.org/3/library/re.html#re.findall re.findall>,
-- or Perl
-- <https://perldoc.perl.org/functions/m.html m//>,
-- or Unix
-- <https://www.gnu.org/software/grep/ grep>.
--
-- __Replace.Attoparsec__ can be used in the same sort of “stream editing”
-- or “search-and-replace” situations in which one would use Python
-- <https://docs.python.org/3/library/re.html#re.sub re.sub>,
-- or Perl
-- <https://perldoc.perl.org/functions/s.html s///>,
-- or Unix
-- <https://www.gnu.org/software/sed/manual/html_node/The-_0022s_0022-Command.html sed>,
-- or
-- <https://www.gnu.org/software/gawk/manual/gawk.html awk>.
--
-- __Replace.Attoparsec__ can be used in the same sort of “string splitting”
-- situations in which one would use Python
-- <https://docs.python.org/3/library/re.html#re.split re.split>
-- or Perl
-- <https://perldoc.perl.org/functions/split.html split>.
--
-- See the __[replace-attoparsec](https://hackage.haskell.org/package/replace-attoparsec)__ package README for usage examples.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Replace.Attoparsec.Text
  (
    -- * Running parser
    --
    -- | Functions in this section are /ways to run parsers/
    -- (like 'Data.Attoparsec.Text.parse'). They take
    -- as arguments a @sep@ parser and some input, run the parser on the input,
    -- and return a result.
    breakCap
  , splitCap
  , streamEdit
  , streamEditT
    -- * Parser combinator
    --
    -- | Functions in this section are /parser combinators/. They take
    -- a @sep@ parser for an argument, combine @sep@ with another parser,
    -- and return a new parser.
  , anyTill
  , sepCap
  , findAll
  , findAllCap
  )
where

import Data.Functor.Identity
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Attoparsec.Internal.Types as AT


-- |
-- === Break on and capture one pattern
--
-- Find the first occurence of a pattern in a text stream, capture the found
-- pattern, and break the input text stream on the found pattern.
--
-- The 'breakCap' function is like 'Data.List.takeWhile', but can be predicated
-- beyond more than just the next one token. It's also like 'Data.Text.breakOn',
-- but the @needle@ can be a pattern instead of a constant string.
--
-- Be careful not to look too far
-- ahead; if the @sep@ parser looks to the end of the input then 'breakCap'
-- could be /O(n²)/.
--
-- The pattern parser @sep@ may match a zero-width pattern (a pattern which
-- consumes no parser input on success).
--
-- ==== Output
--
--  * @Nothing@ when no pattern match was found.
--  * @Just (prefix, parse_result, suffix)@ for the result of parsing the
--    pattern match, and the @prefix@ string before and the @suffix@ string
--    after the pattern match. @prefix@ and @suffix@ may be zero-length strings.
--
-- ==== Access the matched section of text
--
-- If you want to capture the matched string, then combine the pattern
-- parser @sep@ with 'Data.Attoparsec.Text.match'.
--
-- With the matched string, we can reconstruct the input string.
-- For all @input@, @sep@, if
--
-- @
-- let ('Just' (prefix, (infix, _), suffix)) = breakCap ('Data.Attoparsec.Text.match' sep) input
-- @
--
-- then
--
-- @
-- input == prefix '<>' infix '<>' suffix
-- @
breakCap
    :: Parser a
        -- ^ The pattern matching parser @sep@
    -> T.Text
        -- ^ The input stream of text
    -> Maybe (T.Text, a, T.Text)
        -- ^ Maybe (prefix, parse_result, suffix)
breakCap sep input =
    case parseOnly pser input of
        (Left _) -> Nothing
        (Right x) -> Just x
  where
    pser = do
      (prefix, cap) <- anyTill sep
      suffix <- A.takeText
      pure (prefix, cap, suffix)
{-# INLINABLE breakCap #-}

--
-- |
-- === Split on and capture all patterns
--
-- Find all occurences of the pattern @sep@, split the input string, capture
-- all the patterns and the splits.
--
-- The input string will be split on every leftmost non-overlapping occurence
-- of the pattern @sep@. The output list will contain
-- the parsed result of input string sections which match the @sep@ pattern
-- in 'Right', and non-matching sections in 'Left'.
--
-- 'splitCap' depends on 'sepCap', see 'sepCap' for more details.
--
-- ==== Access the matched section of text
--
-- If you want to capture the matched strings, then combine the pattern
-- parser @sep@ with 'Data.Attoparsec.Text.match'.
--
-- With the matched strings, we can reconstruct the input string.
-- For all @input@, @sep@, if
--
-- @
-- let output = splitCap ('Data.Attoparsec.Text.match' sep) input
-- @
--
-- then
--
-- @
-- input == 'Data.Monoid.mconcat' ('Data.Bifunctor.second' 'Data.Tuple.fst' '<$>' output)
-- @
splitCap
    :: Parser a
        -- ^ The pattern matching parser @sep@
    -> T.Text
        -- ^ The input stream of text
    -> [Either T.Text a]
        -- ^ List of matching and non-matching input sections
splitCap sep input = do
    case parseOnly (sepCap sep) input of
        (Left _) -> undefined -- sepCap can never fail
        (Right r) -> r
{-# INLINABLE splitCap #-}


-- |
-- === Stream editor
--
-- Also known as “find-and-replace”, or “match-and-substitute”. Finds all
-- of the sections of the stream which match the pattern @sep@, and replaces
-- them with the result of the @editor@ function.
--
-- ==== Access the matched section of text in the @editor@
--
-- If you want access to the matched string in the @editor@ function,
-- then combine the pattern parser @sep@
-- with 'Data.Attoparsec.Text.match'. This will effectively change
-- the type of the @editor@ function to @(Text,a) -> Text@.
--
-- This allows us to write an @editor@ function which can choose to not
-- edit the match and just leave it as it is. If the @editor@ function
-- returns the first item in the tuple, then @streamEdit@ will not change
-- the matched string.
--
-- So, for all @sep@:
--
-- @
-- streamEdit ('Data.Attoparsec.Text.match' sep) 'Data.Tuple.fst' ≡ 'Data.Function.id'
-- @
streamEdit
    :: Parser a
        -- ^ The pattern matching parser @sep@
    -> (a -> T.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited
    -> T.Text
        -- ^ The edited input stream
streamEdit sep editor = runIdentity . streamEditT sep (Identity . editor)
{-# INLINABLE streamEdit #-}


-- |
-- === Stream editor
--
-- Monad transformer version of 'streamEdit'.
--
-- The @editor@ function will run in the underlying monad context.
--
-- If you want to do 'IO' operations in the @editor@ function then
-- run this in 'IO'.
--
-- If you want the @editor@ function to remember some state,
-- then run this in a stateful monad.
streamEditT
    :: Applicative m
    => Parser a
        -- ^ The pattern matching parser @sep@
    -> (a -> m T.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited
    -> m T.Text
        -- ^ The edited input stream
streamEditT sep editor input = do
    case parseOnly (sepCap sep) input of
        (Left err) -> error err
        -- this function should never error, because it only errors
        -- when the 'sepCap' parser fails, and the 'sepCap' parser
        -- can never fail. If this function ever throws an error, please
        -- report that as a bug.
        -- (We don't use MonadFail because Identity is not a MonadFail.)
        (Right r) -> fmap mconcat $ traverse (either pure editor) r
{-# INLINABLE streamEditT #-}


-- |
-- === Specialized <http://hackage.haskell.org/package/parser-combinators/docs/Control-Monad-Combinators.html#v:manyTill_ manyTill_>
--
-- Parser combinator to consume and capture input until the @sep@ pattern
-- matches, equivalent to
-- @'Control.Monad.Combinators.manyTill_' 'Data.Attoparsec.Text.anyChar' sep@.
-- On success, returns the prefix before the pattern match and the parsed match.
--
-- @sep@ may be a zero-width parser, it may succeed without consuming any
-- input.
--
-- This combinator will produce a parser which acts
-- like 'Data.Attoparsec.Text.takeTill' but is predicated beyond more than
-- just the next one token. It is also like
-- 'Data.Attoparsec.Text.takeTill' in that it is a “high performance” parser.
anyTill
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser (T.Text, a) -- ^ parser
anyTill sep = do
    begin <- getOffset
    (end, x) <- go
    prefix <- substring begin end
    pure (prefix, x)
  where
    go = do
        end <- getOffset
        r <- optional $ try sep
        case r of
            Nothing -> atEnd >>= \case
                True -> empty
                False -> anyChar >> go
            Just x -> pure (end, x)


-- |
-- === Separate and capture
--
-- Parser combinator to find all of the non-overlapping ocurrences
-- of the pattern @sep@ in a text stream.
-- The 'sepCap' parser will always consume its entire input and can never fail.
--
-- 'sepCap' is similar to the @sep*@ family of functions found in
-- <http://hackage.haskell.org/package/parser-combinators/docs/Control-Monad-Combinators.html parser-combinators>
-- and
-- <http://hackage.haskell.org/package/parsers/docs/Text-Parser-Combinators.html parsers>,
-- but it returns the parsed result of the @sep@ parser instead
-- of throwing it away.
--
-- ==== Output
--
-- The input stream is separated and output into a list of sections:
--
-- * Sections which can parsed by the pattern @sep@ will be parsed and captured
--   as 'Right'
-- * Non-matching sections of the stream will be captured in 'Left'.
--
-- The output list also has these properties:
--
-- * If the input is @""@ then the output list will be @[]@.
-- * If there are no pattern matches, then
--   the entire input stream will be returned as one non-matching 'Left' section.
-- * The output list will not contain two consecutive 'Left' sections.
--
-- ==== Zero-width matches forbidden
--
-- If the pattern matching parser @sep@ would succeed without consuming any
-- input then 'sepCap' will force it to fail.
-- If we allow @sep@ to match a zero-width pattern,
-- then it can match the same zero-width pattern again at the same position
-- on the next iteration, which would result in an infinite number of
-- overlapping pattern matches.
sepCap
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either T.Text a] -- ^ parser
sepCap sep = getOffset >>= go
  where
    -- the go function will search for the first pattern match,
    -- and then capture the pattern match along with the preceding
    -- unmatched string, and then recurse.
    -- offsetBegin is the position in the buffer after the last pattern
    -- match.
    go !offsetBegin = do
        !offsetThis <- getOffset
        (<|>)
            ( do
                -- http://hackage.haskell.org/package/attoparsec-0.13.2.3/docs/src/Data.Attoparsec.Internal.html#endOfInput
                _ <- endOfInput
                if offsetThis > offsetBegin
                    then
                        -- If we're at the end of the input, then return
                        -- whatever unmatched string we've got since offsetBegin
                        substring offsetBegin offsetThis >>= \s -> pure [Left s]
                    else pure []
            )
            ( do
                -- About 'thisiter':
                -- It looks stupid and introduces a completely unnecessary
                -- Maybe, but when I refactor to eliminate 'thisiter' and
                -- the Maybe then the benchmarks get dramatically worse.
                thisiter <- (<|>)
                    ( do
                        x <- try sep
                        !offsetAfter <- getOffset
                        -- Don't allow a match of a zero-width pattern
                        when (offsetAfter <= offsetThis) empty
                        return $ Just (x, offsetAfter)
                    )
                    (anyChar >> return Nothing)
                case thisiter of
                    (Just (x, !offsetAfter)) | offsetThis > offsetBegin -> do
                        -- we've got a match with some preceding unmatched string
                        unmatched <- substring offsetBegin offsetThis
                        (Left unmatched:) <$> (Right x:) <$> go offsetAfter
                    (Just (x, !offsetAfter)) -> do
                        -- we're got a match with no preceding unmatched string
                        (Right x:) <$> go offsetAfter
                    Nothing -> go offsetBegin -- no match, try again
            )
{-# INLINABLE sepCap #-}


-- |
-- === Find all occurences, parse and capture pattern matches
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Data.Attoparsec.Text.match' combinator so that
-- the text which matched the pattern parser @sep@ will be returned in
-- the 'Right' sections, along with the result of the parse of @sep@.
--
-- Definition:
--
-- @
-- findAllCap sep = 'sepCap' ('Data.Attoparsec.Text.match' sep)
-- @
findAllCap
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either T.Text (T.Text, a)] -- ^ parser
findAllCap sep = sepCap (match sep)
{-# INLINABLE findAllCap #-}
{-# DEPRECATED findAllCap "replace with `findAllCap sep = sepCap (match sep)`" #-}

-- |
-- === Find all occurences
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Data.Attoparsec.Text.match' combinator and
-- return the text which matched the pattern parser @sep@ in
-- the 'Right' sections.
--
-- Definition:
--
-- @
-- findAll sep = (fmap.fmap) ('Data.Bifunctor.second' fst) $ 'sepCap' ('Data.Attoparsec.Text.match' sep)
-- @
findAll
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either T.Text T.Text] -- ^ parser
findAll sep = (fmap.fmap) (second fst) $ sepCap (match sep)
{-# INLINABLE findAll #-}
{-# DEPRECATED findAll "replace with `findAll sep = (fmap.fmap) (second fst) $ sepCap (match sep)`" #-}


-- Get the 'Data.Attoparsec.Internal.Types.Parser' current offset
-- 'Data.Attoparsec.Internal.Types.Pos' in the stream.
--
-- Note that this is not the number of 'Data.Char's which have been consumed,
-- rather it is an offset into the underlying 'Data.Text.Internal.Text'
-- array buffer, so you cannot use it as an argument to 'Data.Text.index'.
-- But you /can/ use it as an argument to 'Data.Text.Internal.text'.
--
-- [“… you know you're in an uncomfortable state of sin :-)” — bos](https://github.com/bos/attoparsec/issues/101)
getOffset :: Parser Int
getOffset = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)
{-# INLINABLE getOffset #-}


-- Extract a substring from part of the buffer that we've already visited.
--
-- The idea here is that we go back and run the parser 'take' at the Pos
-- which we saved from before, and then we continue from the current Pos,
-- hopefully without messing up the internal parser state.
-- http://hackage.haskell.org/package/attoparsec-0.13.2.3/docs/src/Data.Attoparsec.Text.Internal.html#take
--
-- Should be equivalent to the unexported function
-- http://hackage.haskell.org/package/attoparsec-0.13.2.3/docs/src/Data.Attoparsec.Text.Internal.html#substring
--
-- This is a performance optimization for gathering the unmatched
-- sections of the input. The alternative is to accumulate unmatched
-- characters one anyChar at a time in a list of [Char] and then pack
-- them into a Text.
substring :: Int -> Int -> Parser T.Text
substring !bgn !end = AT.Parser $ \t pos more lose succes ->
    let succes' _t _pos _more a = succes t pos more a
    in
    AT.runParser (takeCheat (end - bgn)) t (AT.Pos bgn) more lose succes'
  where
    -- Dear reader, you deserve an explanation for 'takeCheat'. The
    -- alternative to running 'takeCheat' here would be the following line:
    --
    -- AT.runParser (A.take (end - bgn)) t (AT.Pos bgn) more lose succes'
    --
    -- But 'Attoparsec.take' is not correct, and 'takeCheat' is correct.
    -- It is correct because the Pos which we got from 'getOffset' is an
    -- index into the underlying Data.Text.Array, so (end - bgn) is
    -- in units of the length of the Data.Text.Array, not in units of the
    -- number of Chars.
    --
    -- Furthermore 'takeCheat' is a lot faster because 'A.take' takes a
    -- number of Chars and then iterates over the Text by the number
    -- of Chars, advancing by 4 bytes when it encounters a wide Char.
    -- So, O(N). takeCheat is O(1).
    --
    -- This will be fine as long as we always call 'takeCheat' on the
    -- immutable, already-visited part of the Attoparsec.Text.Buffer's
    -- Data.Text.Array. Which we do.
    --
    -- It's named 'takeCheat' because we're getting access to
    -- the Attoparsec.Text.Buffer through the Data.Text.Internal
    -- interface, even though Attoparsec is extremely vigilant about
    -- not exposing its buffers.
    --
    -- http://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Internal.html
    -- takeCheat :: Int -> Parser T.Text
    takeCheat len = do
        (TI.Text arr off _len) <- A.take 0
        return (TI.Text arr off len)
