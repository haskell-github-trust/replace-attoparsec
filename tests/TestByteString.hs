{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

module TestByteString ( tests ) where

import Data.Attoparsec.ByteString as A
import Data.Attoparsec.Combinator
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import "parsers" Text.Parser.Token
import Replace.Attoparsec.ByteString
import Control.Applicative
import Test.Hspec (describe, shouldBe, it, SpecWith)
import Test.HUnit (assertFailure)

findAllCap' :: Parser a -> Parser [Either B.ByteString (B.ByteString, a)]
findAllCap' sep = sepCap (match sep)

tests :: SpecWith ()
tests = describe "input ByteString" do
    runParserTest "findAllCap upperChar"
        (findAllCap' upperChar)
        ("aBcD" :: B.ByteString)
        [Left "a", Right ("B", c2w 'B'), Left "c", Right ("D", c2w 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    runParserTest "zero-consumption parser"
        (sepCap (many upperChar))
        ("aBcD" :: B.ByteString)
        [Left "a", Right [c2w 'B'], Left "c", Right [c2w 'D']]
    runParserTest "scinum"
        (sepCap scinum)
        "1E3"
        [Right (1,3)]
    runParserTest "monad fail"
        (sepCap (fail "" :: Parser ()))
        "xxx"
        [Left "xxx"]
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
    runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        ("a")
        ([Left "a"])
#endif
    runParserFeed "const string"
        (sepCap (string "aa"))
        " a" "a "
        [Left " ",Right"aa",Left" "]
    runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
    streamEditTest "empty input" (match (fail "")) fst "" ""
    breakCapTest "basic" upperChar "aAa" (Just ("a", c2w 'A', "a"))
    breakCapTest "first" upperChar "Aa" (Just ("", c2w 'A', "a"))
    breakCapTest "last" upperChar "aA" (Just ("a", c2w 'A', ""))
    breakCapTest "fail" upperChar "aaa" Nothing
    breakCapTest "match" (match upperChar) "aAa" (Just ("a", ("A",c2w 'A'), "a"))
    breakCapTest "zero-width" (lookAhead upperChar) "aAa" (Just ("a", c2w 'A', "Aa"))
    breakCapTest "empty input" upperChar "" Nothing
    breakCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))

  where
    runParserTest nam p input expected = it nam $ shouldBe
        (parseOnly p input) (Right expected)

    runParserFeed nam p input1 input2 expected = it nam do
        case parse p input1 of
            A.Fail _i _ e -> assertFailure $ show e
            A.Partial cont1 -> case cont1 input2 of
                A.Fail _i _ e -> assertFailure $ show e
                A.Partial cont2 -> case cont2 "" of
                    A.Fail _i _ e -> assertFailure $ show e
                    A.Partial _ -> assertFailure "Should not ask for more input"
                    A.Done _i output -> output `shouldBe` expected
                A.Done _i _output -> assertFailure "Should ask for more input"
            A.Done _i _output -> assertFailure "Should ask for more input"

    streamEditTest nam sep editor input expected = it nam $ shouldBe
        (streamEdit sep editor input) expected

    breakCapTest nam sep input expected = it nam $ shouldBe
        (breakCap sep input) expected

    scinum :: Parser (Double, Integer)
    scinum = do
        m <- (fromIntegral :: Integer -> Double) <$> decimal
        _ <- string "E"
        e <- decimal
        return (m, e)

    upperChar = satisfy $ \c -> c >= c2w 'A' && c <= c2w 'Z'

