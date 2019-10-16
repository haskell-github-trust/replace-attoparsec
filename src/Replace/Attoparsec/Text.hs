-- |
-- Module    : Replace.Attoparsec.Text
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

module Replace.Attoparsec.Text
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
import Data.Foldable
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Attoparsec.Internal.Types as AT
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

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
    -> Parser [Either T.Text a]
sepCap sep = (fmap.fmap) (first T.pack)
             $ fmap sequenceLeft
             $ many $ fmap Right (consumeSome sep) <|> fmap Left anyChar
  where
    sequenceLeft :: [Either Char r] -> [Either [Char] r]
    sequenceLeft = foldr consLeft []
      where
        consLeft :: Either l r -> [Either [l] r] -> [Either [l] r]
        consLeft (Left l) ((Left ls):xs) = {-# SCC consLeft #-} (Left (l:ls)):xs
        consLeft (Left l) xs = {-# SCC consLeft #-} (Left [l]):xs
        consLeft (Right r) xs = {-# SCC consLeft #-} (Right r):xs
    -- If sep succeeds and consumes 0 input tokens, we must force it to fail,
    -- otherwise infinite loop
    consumeSome p = {-# SCC consumeSome #-} do
        offset1 <- getOffset
        x <- {-# SCC sep #-} p
        offset2 <- getOffset
        when (offset1 >= offset2) empty
        return x
{-# INLINABLE sepCap #-}

-- |
-- == Find all occurences, parse and capture pattern matches
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
    -> Parser [Either T.Text (T.Text, a)]
findAllCap sep = sepCap (match sep)
{-# INLINABLE findAllCap #-}

-- |
-- == Find all occurences
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
    -> Parser [Either T.Text T.Text]
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
-- a “way to run a parser”, like 'Data.Attoparsec.Text.parse'
-- or 'Data.Attoparsec.Text.parseOnly'.
--
-- === Access the matched section of text in the @editor@
--
-- If you want access to the matched string in the @editor@ function,
-- then combine the pattern parser @sep@
-- with 'Data.Attoparsec.Text.match'. This will effectively change
-- the type of the @editor@ function to @(Text,a) -> Text@.
--
-- This allows us to write an @editor@ function which can choose to not
-- edit the match and just leave it as it is. If the @editor@ function
-- always returns the first item in the tuple, then @streamEdit@ changes
-- nothing.
--
-- So, for all @sep@:
--
-- @
-- streamEdit ('Data.Attoparsec.Text.match' sep) 'Data.Tuple.fst' ≡ 'Data.Function.id'
-- @
streamEdit
    :: Parser a
        -- ^ The parser @sep@ for the pattern of interest.
    -> (a -> T.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited.
    -> T.Text
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
    -> (a -> m T.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited.
    -> m T.Text
streamEditT sep editor input =
    case parseOnly (sepCapRaw sep) input of
        (Left err) -> error err
        -- this function should never error, because it only errors
        -- when the 'sepCap' parser fails, and the 'sepCap' parser
        -- can never fail. If this function ever throws an error, please
        -- report that as a bug.
        -- (We don't use MonadFail because Identity is not a MonadFail.)
        (Right r) -> TL.toStrict <$> TLB.toLazyText <$> foldrM buildText mempty r
        -- Omitting the TL.toStrict and returning lazy text and
        -- printing with Data.Text.Lazy.IO.putStr doesn't speed it up.
  where
    -- A version of sepCap which doesn't mconcat the non-matching characters
    -- with `sequenceLeft`.
    sepCapRaw
        :: Parser a -- ^ The pattern matching parser @sep@
        -> Parser [Either Char a]
    sepCapRaw sep' = many $ fmap Right (consumeSome sep') <|> fmap Left anyChar
    -- Prepend the result to a builder, appropriate for foldr, see
    -- https://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Builder.html
    buildText (Left c) builder = return $ TLB.singleton c <> builder
    buildText (Right x) builder = flip mappend builder <$> TLB.fromText <$> editor x
    -- If sep succeeds and consumes 0 input tokens, we must force it to fail,
    -- otherwise infinite loop
    consumeSome p = {-# SCC consumeSome #-} do
        offset1 <- getOffset
        x <- {-# SCC sep #-} p
        offset2 <- getOffset
        when (offset1 >= offset2) empty
        return x
{-# INLINABLE streamEditT #-}

-- | Get the 'Data.Attoparsec.Text.Parser' ’s current offset position in the stream.
--
-- [“… you know you're in an uncomfortable state of sin :-)” — bos](https://github.com/bos/attoparsec/issues/101)
getOffset :: Parser Int
getOffset = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)
{-# INLINABLE getOffset #-}


-- This is the non-specialized version of streamEditT' which uses `sepCap`.
-- It is unexported in this library, but left in the source code for comparison.
-- This version is faster for the "sparse" benchmark, but the specialized
-- version is faster for the "dense" benchmark. See replace-benchmark.
_streamEditT
    :: (Monad m)
    => Parser a
        -- ^ The parser @sep@ for the pattern of interest.
    -> (a -> m T.Text)
        -- ^ The @editor@ function. Takes a parsed result of @sep@
        -- and returns a new stream section for the replacement.
    -> T.Text
        -- ^ The input stream of text to be edited.
    -> m T.Text
_streamEditT sep editor input = do
    case parseOnly (sepCap sep) input of
        (Left err) -> error err
        -- this function should never error, because it only errors
        -- when the 'sepCap' parser fails, and the 'sepCap' parser
        -- can never fail. If this function ever throws an error, please
        -- report that as a bug.
        -- (We don't use MonadFail because Identity is not a MonadFail.)
        (Right r) -> fmap mconcat $ traverse (either return editor) r

