-- {-# LANGUAGE RankNTypes #-}
module Replace.Attoparsec.ByteString
    ( sepCap
    , findAllCap
    , getOffset
    )
where

import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import GHC.Word
import qualified Data.Attoparsec.Internal.Types as AT

sepCap
    :: Parser a
    -> Parser [Either BS.ByteString a]
sepCap sep = (fmap.fmap) (first BS.pack)
             $ fmap sequenceLeft
             $ many $ fmap Right (consumeSome sep) <|> fmap Left anyWord8
  where
    sequenceLeft :: [Either Word8 r] -> [Either [Word8] r]
    sequenceLeft = foldr consLeft []
      where
        consLeft :: Either l r -> [Either [l] r] -> [Either [l] r]
        consLeft (Left l) ((Left ls):xs) = (Left (l:ls)):xs
        consLeft (Left l) xs = (Left [l]):xs
        consLeft (Right r) xs = (Right r):xs
    -- If sep succeeds and consumes 0 input tokens, we must force it to fail,
    -- otherwise infinite loop
    consumeSome p = do
        offset1 <- getOffset
        x <- p
        offset2 <- getOffset
        when (offset1 >= offset2) empty
        return x

-- | Get the Parser's current offset position in the stream.
--
-- [“… you know you're in an uncomfortable state of sin :-)” — bos](https://github.com/bos/attoparsec/issues/101)
getOffset :: Parser Int
getOffset = AT.Parser $ \t pos more _ succ' -> succ' t pos more (AT.fromPos pos)

findAllCap
    :: Parser a -- ^ The pattern matching parser @sep@
    -> Parser [Either BS.ByteString (BS.ByteString, a)]
findAllCap sep = sepCap (match sep)
