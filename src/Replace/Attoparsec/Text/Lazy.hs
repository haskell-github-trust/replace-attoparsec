-- |
-- Module    : Replace.Attoparsec.Text.Lazy
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Replace.Attoparsec.Text.Lazy
  (
    -- * Running parser
    --
    -- | Functions in this section are /ways to run parsers/
    -- (like 'Data.Attoparsec.Text.Lazy.parse'). They take
    -- as arguments a @sep@ parser and some input, run the parser on the input,
    -- and return a result.
    streamEdit
  , streamEditT
    -- * Parser combinator
    --
    -- | Functions in this section are /parser combinators/. They take
    -- a @sep@ parser for an argument, combine @sep@ with another parser,
    -- and return a new parser.
  , anyTill
  )
where

import Data.Functor.Identity
import Control.Applicative
import Data.Attoparsec.Text.Lazy as A hiding (parseOnly)
import qualified Data.Attoparsec.Text as AS
import Data.List as List ( intercalate )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Internal.Lazy as TI
import qualified Data.Text as TS
import qualified Data.Text.Internal as TIS
import qualified Data.Attoparsec.Internal.Types as AT
import Data.Coerce

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
--
-- ==== Laziness
--
-- This is lazy in the input text chunks and should release processed chunks to
-- the garbage collector promptly.
--
-- The output is constructed by a 'TB.Builder' and is subject to the chunk size
-- used there.
streamEdit
    :: forall a. Parser a
        -- ^ The pattern matching parser @sep@
    -> (a -> TS.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited
    -> T.Text
        -- ^ The edited input stream
streamEdit = coerce (streamEditT @Identity @a)
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
--
-- ==== Laziness
--
-- This is lazy in the input text chunks and should release processed chunks to
-- the garbage collector promptly, i.e. as soon as the presence of a @sep@ has
-- been ruled out.
--
-- Note that this is as only as lazy in the chunks as the selected monad allows
-- it to be, i.e. if your monad requires running the entire computation before
-- getting the result then this is effectively strict in the input stream.
--
-- The output is constructed by a 'TB.Builder' and is subject to the chunk size
-- used there.
streamEditT
    :: (Applicative m)
    => Parser a
        -- ^ The pattern matching parser @sep@
    -> (a -> m TS.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited
    -> m T.Text
        -- ^ The edited input stream
streamEditT sep editor = fmap TB.toLazyText . go mempty defP
  where
    -- Our starting parser
    defP = AS.parse (anyTill sep)

    go failRet p input = case input of
      -- We didn't find anything by the end of the stream, return the accumulated
      -- failure text
      TI.Empty      -> pure failRet
      TI.Chunk c cs -> case p c of
        -- We didn't find sep or the beginning of sep in this chunk, return the
        -- accumulated failure text as well as this chunk, followed by the
        -- continued edited stream
        AS.Fail{}      -> (failRet <>) . (TB.fromText c <>) <$> go mempty defP cs
        -- We found the beginning of sep, add to the failure text in case this
        -- isn't really sep and recurse on the remainder of the stream
        AS.Partial f   -> go (failRet <> TB.fromText c) f cs
        -- We found sep, return the concatenation of the text until sep, the
        -- edited sep and the edited rest of the stream.
        AS.Done next r -> mconcat <$> sequenceA
          [ pure (TB.fromLazyText (fst r))
          , TB.fromText <$> editor (snd r)
          , go mempty defP (TI.chunk next cs)
          ]
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
--
-- ==== Laziness
--
-- When the 'anyTill' parser reaches the end of the current input chunk
-- before finding the beginning of @sep@ then the parser will fail.
--
-- When the 'anyTill' parser reaches the end of the current input chunk
-- while it is successfully parsing @sep@ then it will lazily fetch more
-- input and continue parsing.
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
            Nothing -> atChunkEnd >>= \case
                True -> empty
                False -> anyChar >> go
            Just x -> pure (end, x)

-- | Always succeeds, returns 'True' if the parser is at the end of the current
-- buffer and any additional input would require a 'TI.Partial' result.
atChunkEnd :: Parser Bool
atChunkEnd = AT.Parser $ \t pos more _lose succ' ->
  succ' t pos more (pos + 1 == AT.atBufferEnd (undefined :: TS.Text) t)

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
    let succes' _t _pos _more a = succes t pos more (T.fromStrict a)
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
    takeCheat :: Int -> Parser TS.Text
    takeCheat len = do
        (TIS.Text arr off _len) <- A.take 0
        return (TIS.Text arr off len)


--
-- These are from the latest version of attoparsec, remove them when bumping it
-- to 0.14.0 or later and use A.parseOnly instead
--

-- | Convert a 'Result' value to an 'Either' value.
eitherResult' :: Result r -> Either String r
eitherResult' (Done _ r)        = Right r
eitherResult' (Fail _ [] msg)   = Left msg
eitherResult' (Fail _ ctxs msg) = Left (List.intercalate " > " ctxs ++ ": " ++ msg)

-- | Run a parser and convert its 'Result' to an 'Either' value.
--
-- This function does not force a parser to consume all of its input.
-- Instead, any residual input will be discarded.  To force a parser
-- to consume all of its input, use something like this:
--
-- @
--'parseOnly' (myParser 'Control.Applicative.<*' 'endOfInput')
-- @
parseOnly :: A.Parser a -> T.Text -> Either String a
parseOnly p = eitherResult' . parse p
{-# INLINE parseOnly #-}
