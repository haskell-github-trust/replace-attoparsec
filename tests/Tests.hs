{-# LANGUAGE BlockArguments #-}

module Main ( main ) where

import Test.Hspec
import TestByteString
import TestText
import TestTextLazy

main :: IO ()
main = hspec do
  TestByteString.tests
  TestText.tests
  TestTextLazy.tests