-- |
-- Module    : Replace.Attoparsec.ByteString
-- Copyright : ©2019 James Brock
-- License   : BSD2
-- Maintainer: James Brock <jamesbrock@gmail.com>
--
-- __Replace.Attoparsec__ is for finding text patterns, and also editing and
-- replacing the found patterns.
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
-- See the __[replace-attoparsec](https://hackage.haskell.org/package/replace-attoparsec)__ package README for usage examples.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Replace.Attoparsec.ByteString
  (
    -- * Parser combinator
    sepCap
  , findAll
  , findAllCap

    -- * Running parser
  , streamEdit
  , streamEditT

    -- * Parser
  , getOffset
  )
where

import Data.Functor.Identity
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.Attoparsec.Internal.Types as AT

-- |
-- == Separate and capture
--
-- Parser combinator to find all of the non-overlapping ocurrences
-- of the pattern @sep@ in a text stream. Separate the stream into sections:
--
-- * sections which can parsed by the pattern @sep@ will be captured as
--   matching sections in 'Right'
-- * non-matching sections of the stream will be captured in 'Left'.
--
-- This parser will always consume its entire input and can never fail.
-- If there are no pattern matches, then the entire input stream will be
-- returned as a non-matching 'Left' section.
--
-- The pattern matching parser @sep@ will not be allowed to succeed without
-- consuming any input. If we allow the parser to match a zero-width pattern,
-- then it can match the same zero-width pattern again at the same position
-- on the next iteration, which would result in an infinite number of
-- overlapping pattern matches. So, for example, the
-- pattern @many digit@, which can match zero occurences of a digit,
-- will be treated by @sepCap@ as @many1 digit@, and required to match
-- at least one digit.
--
-- This @sepCap@ parser combinator is the basis for all of the other
-- features of this module. It is similar to the @sep*@ family of functions
-- found in
-- <http://hackage.haskell.org/package/parser-combinators/docs/Control-Monad-Combinators.html parser-combinators>
-- and
-- <http://hackage.haskell.org/package/parsers/docs/Text-Parser-Combinators.html parsers>
-- but, importantly, it returns the parsed result of the @sep@ parser instead
-- of throwing it away.
--
sepCap
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either B.ByteString a]
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
                        x <- sep
                        !offsetAfter <- getOffset
                        -- Don't allow a match of a zero-width pattern
                        when (offsetAfter <= offsetThis) empty
                        return $ Just (x, offsetAfter)
                    )
                    (advance >> return Nothing)
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
    -- Using this advance function instead of 'anyWord8' seems to give us
    -- a 5%-20% performance improvement.
    --
    -- It's safe to use 'advance' because after 'advance' we always check
    -- for 'endOfInput' before trying to read anything from the buffer.
    --
    -- http://hackage.haskell.org/package/attoparsec-0.13.2.3/docs/src/Data.Attoparsec.ByteString.Internal.html#anyWord8
    -- http://hackage.haskell.org/package/attoparsec-0.13.2.3/docs/src/Data.Attoparsec.ByteString.Internal.html#advance
    -- advance :: Parser ()
    advance = AT.Parser $ \t pos more _lose succes ->
        succes t (pos + AT.Pos 1) more ()

    -- Extract a substring from part of the buffer that we've already visited.
    -- Does not check bounds.
    --
    -- The idea here is that we go back and run the parser 'take' at the Pos
    -- which we saved from before, and then we continue from the current Pos,
    -- hopefully without messing up the internal parser state.
    --
    -- Should be equivalent to the unexported function
    -- Data.Attoparsec.ByteString.Buffer.substring
    -- http://hackage.haskell.org/package/attoparsec-0.13.2.3/docs/src/Data.Attoparsec.ByteString.Buffer.html#substring
    --
    -- This is a performance optimization for gathering the unmatched
    -- sections of the input. The alternative is to accumulate unmatched
    -- characters one anyWord8 at a time in a list of [Word8] and then pack
    -- them into a ByteString.
    substring :: Int -> Int -> Parser B.ByteString
    substring !pos1 !pos2 = AT.Parser $ \t pos more lose succes ->
        let succes' _t _pos _more a = succes t pos more a
        in AT.runParser (A.take (pos2 - pos1)) t (AT.Pos pos1) more lose succes'
{-# INLINABLE sepCap #-}

-- |
-- == Find all occurences, parse and capture pattern matches
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Data.Attoparsec.ByteString.match' combinator so that
-- the text which matched the pattern parser @sep@ will be returned in
-- the 'Right' sections, along with the result of the parse of @sep@.
--
-- Definition:
--
-- @
-- findAllCap sep = 'sepCap' ('Data.Attoparsec.ByteString.match' sep)
-- @
findAllCap
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either B.ByteString (B.ByteString, a)]
findAllCap sep = sepCap (match sep)
{-# INLINABLE findAllCap #-}


-- |
-- == Find all occurences
--
-- Parser combinator for finding all occurences of a pattern in a stream.
--
-- Will call 'sepCap' with the 'Data.Attoparsec.ByteString.match' combinator and
-- return the text which matched the pattern parser @sep@ in
-- the 'Right' sections.
--
-- Definition:
--
-- @
-- findAll sep = (fmap.fmap) ('Data.Bifunctor.second' fst) $ 'sepCap' ('Data.Attoparsec.ByteString.match' sep)
-- @
findAll
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either B.ByteString B.ByteString]
findAll sep = (fmap.fmap) (second fst) $ sepCap (match sep)
{-# INLINABLE findAll #-}


-- |
-- == Stream editor
--
-- Also known as “find-and-replace”, or “match-and-substitute”. Finds all
-- of the sections of the stream which match the pattern @sep@, and replaces
-- them with the result of the @editor@ function.
--
-- This function is not a “parser combinator,” it is
-- a “way to run a parser”, like 'Data.Attoparsec.ByteString.parse'
-- or 'Data.Attoparsec.ByteString.parseOnly'.
--
-- === Access the matched section of text in the @editor@
--
-- If you want access to the matched string in the @editor@ function,
-- then combine the pattern parser @sep@
-- with 'Data.Attoparsec.ByteString.match'. This will effectively change
-- the type of the @editor@ function to @(ByteString,a) -> ByteString@.
--
-- This allows us to write an @editor@ function which can choose to not
-- edit the match and just leave it as it is. If the @editor@ function
-- always returns the first item in the tuple, then @streamEdit@ changes
-- nothing.
--
-- So, for all @sep@:
--
-- @
-- streamEdit ('Data.Attoparsec.ByteString.match' sep) 'Data.Tuple.fst' ≡ 'Data.Function.id'
-- @
streamEdit
    -- :: forall s a. (Stream s, Monoid s, Tokens s ~ s, Show s, Show (Token s), Typeable s)
    :: Parser a
        -- ^ The parser @sep@ for the pattern of interest.
    -> (a -> B.ByteString)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> B.ByteString
        -- ^ The input stream of text to be edited.
    -> B.ByteString
streamEdit sep editor = runIdentity . streamEditT sep (Identity . editor)
{-# INLINABLE streamEdit #-}

-- |
-- == Stream editor transformer
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
    :: (Monad m)
    => Parser a
        -- ^ The parser @sep@ for the pattern of interest.
    -> (a -> m B.ByteString)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> B.ByteString
        -- ^ The input stream of text to be edited.
    -> m B.ByteString
streamEditT sep editor input = do
    case parseOnly (sepCap sep) input of
        (Left err) -> error err
        -- this function should never error, because it only errors
        -- when the 'sepCap' parser fails, and the 'sepCap' parser
        -- can never fail. If this function ever throws an error, please
        -- report that as a bug.
        -- (We don't use MonadFail because Identity is not a MonadFail.)
        (Right r) -> fmap mconcat $ traverse (either return editor) r
{-# INLINABLE streamEditT #-}

-- | Get the 'Data.Attoparsec.ByteString.Parser' ’s current offset position in the stream.
--
-- [“… you know you're in an uncomfortable state of sin :-)” — bos](https://github.com/bos/attoparsec/issues/101)
getOffset :: Parser Int
getOffset = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

