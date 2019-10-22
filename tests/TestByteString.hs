{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module TestByteString ( tests ) where

import Distribution.TestSuite as TestSuite
import Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import "parsers" Text.Parser.Token
import Replace.Attoparsec.ByteString
import Control.Applicative


tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAll upperChar"
        (findAllCap upperChar)
        ("aBcD" :: B.ByteString)
        [Left "a", Right ("B", c2w 'B'), Left "c", Right ("D", c2w 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many upperChar))
        ("aBcD" :: B.ByteString)
        [Left "a", Right [c2w 'B'], Left "c", Right [c2w 'D']]
    , Test $ runParserTest "scinum"
        (sepCap scinum)
        ("1E3")
        ([Right (1,3)])
    , Test $ runParserTest "getOffset"
        (sepCap offsetA)
        ("xxAxx")
        ([Left "xx", Right 2, Left "xx"])
    , Test $ runParserTest "monad fail"
        (sepCap (fail "" :: Parser ()))
        ("xxx")
        ([Left "xxx"])
    , Test $ runParserTest "read fail"
        (sepCap (return (read "a" :: Int) :: Parser Int))
        ("a")
        ([Left "a"])
    , Test $ runParserFeed "const string"
        (sepCap (string "aa"))
        (" a") ("a ")
        ([Left " ",Right"aa",Left" "])
    , Test $ streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    , Test $ streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    , Test $ streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
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
                                        $ show output ++ " ≠ " ++ show expected)
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
                                                $ show output ++ " ≠ " ++ show expected)
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
                                $ show output ++ " ≠ " ++ show expected)
            , name = "streamEdit " ++ nam
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

    upperChar = satisfy $ \c -> c >= c2w 'A' && c <= c2w 'Z'

    offsetA :: Parser Int
    offsetA = getOffset <* string "A"

