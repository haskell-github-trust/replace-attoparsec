{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module TestText ( tests ) where

import Distribution.TestSuite as TestSuite
import Replace.Attoparsec.Text
import Data.Attoparsec.Text as A
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import Text.Parser.Char (upper)
import Control.Applicative

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap (upper :: Parser Char))
        ("aBcD" :: T.Text)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upper :: Parser Char)))
        ("aBcD" :: T.Text)
        [Left "a", Right "B", Left "c", Right "D"]
    , Test $ runParserTest "scinum"
        (sepCap scinum)
        ("1E3")
        ([Right (1,3)])
    , Test $ runParserTest "monad fail"
        (sepCap (fail "" :: Parser ()))
        ("xxx")
        ([Left "xxx"])
#if MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
    , Test $ runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        ("a")
        ([Left "a"])
#endif
    , Test $ runParserTest "findAll astral"
        (findAll ((A.takeWhile (=='𝅘𝅥𝅯') :: Parser T.Text)))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :: T.Text)
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    , Test $ runParserFeed "const string"
        (sepCap (string "aa"))
        (" a") ("a ")
        ([Left " ",Right"aa",Left" "])
    , Test $ runParserFeed "findAll astral"
        (findAll ((A.takeWhile (=='𝅘𝅥𝅯') :: Parser T.Text)))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯") ("𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :: T.Text)
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    , Test $ runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    , Test $ streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    , Test $ streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    , Test $ streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
    , Test $ streamEditTest "empty input" (match (fail "")) (fst) "" ""
    , Test $ breakCapTest "basic" upper "aAa" (Just ("a", 'A', "a"))
    , Test $ breakCapTest "first" upper "Aa" (Just ("", 'A', "a"))
    , Test $ breakCapTest "last" upper "aA" (Just ("a", 'A', ""))
    , Test $ breakCapTest "fail" upper "aaa" Nothing
    , Test $ breakCapTest "match" (match upper) "aAa" (Just ("a", ("A",'A'), "a"))
    , Test $ breakCapTest "zero-width" (lookAhead upper) "aAa" (Just ("a",'A', "Aa"))
    , Test $ breakCapTest "empty input" upper "" Nothing
    , Test $ breakCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))
    ]
  where
    runParserTest nam p input expected = TestInstance
            { run = do
                case parseOnly p input of
                    Left e -> return (Finished $ TestSuite.Fail $ show e)
                    Right output ->
                        if (output == expected)
                            then return (Finished Pass)
                            else return (Finished $ TestSuite.Fail
                                $ "got " <> show output <> " expected " <> show expected)
            , name = "parseOnly sepCap " <> nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    runParserFeed nam p input1 input2 expected = TestInstance
            { run = do
                case parse p input1 of
                    A.Fail _i _ e -> return (Finished $ TestSuite.Fail $ show e)
                    A.Partial cont1 -> case cont1 input2 of
                        A.Fail _i _ e -> return (Finished $ TestSuite.Fail $ show e)
                        A.Partial cont2 -> case cont2 "" of
                            A.Fail _i _ e -> return (Finished $ TestSuite.Fail $ show e)
                            A.Partial _ -> return (Finished $ TestSuite.Fail $ "Should not ask for more input")
                            A.Done _i output ->
                                if (output == expected)
                                    then return (Finished Pass)
                                    else return (Finished $ TestSuite.Fail
                                        $ "got " <> show output <> " expected " <> show expected)
                        A.Done _i _output -> return (Finished $ TestSuite.Fail $ "Should ask for more input")
                    A.Done _i _output -> return (Finished $ TestSuite.Fail $ "Should ask for more input")
            , name = "parse Partial sepCap " <> nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    streamEditTest nam sep editor input expected = TestInstance
            { run = do
                let output = streamEdit sep editor input
                if (output == expected)
                    then return (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                        $ "got " <> show output <> " expected " <> show expected)
            , name = "streamEdit " ++ nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    breakCapTest nam sep input expected = TestInstance
            { run = do
                let output = breakCap sep input
                if (output == expected)
                    then return (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                        $ "got " <> show output <> " expected " <> show expected)
            , name = "breakCap " ++ nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

    scinum :: Parser (Double, Integer)
    scinum = do
        m <- (fromIntegral :: Integer -> Double) <$> decimal
        _ <- string "E"
        e <- decimal
        return (m, e)


