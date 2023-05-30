{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

module TestText ( tests ) where

import Replace.Attoparsec.Text
import Data.Attoparsec.Text as A
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import Text.Parser.Char (upper)
import Control.Applicative
import Data.Bifunctor
import qualified TestTextLazy
import Test.Hspec (describe, shouldBe, it, SpecWith)
import Test.HUnit (assertFailure)

findAllCap' :: Parser a -> Parser [Either T.Text (T.Text, a)]
findAllCap' sep = sepCap (match sep)
findAll' :: Parser b -> Parser [Either T.Text T.Text]
findAll' sep = fmap (second fst) <$> sepCap (match sep)

tests :: SpecWith ()
tests = describe "input Text" do
    runParserTest "findAllCap upperChar"
        (findAllCap' (upper :: Parser Char))
        ("aBcD" :: T.Text)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    runParserTest "zero-consumption parser"
        (sepCap (many (upper :: Parser Char)))
        ("aBcD" :: T.Text)
        [Left "a", Right "B", Left "c", Right "D"]
    runParserTest "scinum"
        (sepCap scinum)
        "1E3"
        [Right (1,3)]
    runParserTest "monad fail"
        (sepCap (fail "" :: Parser ()))
        "xxx"
        [Left "xxx"]

    runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        "a"
        [Left "a"]

    runParserTest "findAll astral"
        (findAll' (A.takeWhile (=='𝅘𝅥𝅯') :: Parser T.Text))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :: T.Text)
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    runParserFeed "const string"
        (sepCap (string "aa"))
        " a" "a "
        [Left " ",Right "aa",Left " "]
    runParserFeed "findAll astral"
        (findAll' (A.takeWhile (=='𝅘𝅥𝅯') :: Parser T.Text))
        "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯" ("𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :: T.Text)
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
    streamEditTest "empty input" (match (fail "")) fst "" ""
    breakCapTest "basic" upper "aAa" (Just ("a", 'A', "a"))
    breakCapTest "first" upper "Aa" (Just ("", 'A', "a"))
    breakCapTest "last" upper "aA" (Just ("a", 'A', ""))
    breakCapTest "fail" upper "aaa" Nothing
    breakCapTest "match" (match upper) "aAa" (Just ("a", ("A",'A'), "a"))
    breakCapTest "zero-width" (lookAhead upper) "aAa" (Just ("a",'A', "Aa"))
    breakCapTest "empty input" upper "" Nothing
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


