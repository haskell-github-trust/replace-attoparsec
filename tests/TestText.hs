{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestText ( tests ) where

import Distribution.TestSuite as TestSuite
import Replace.Attoparsec.Text
import Data.Attoparsec.Text
-- import Data.Attoparsec.Text.Char
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
    , Test $ streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
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
            , name = "sepCap " ++ nam
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


    offsetA :: Parser Int
    offsetA = getOffset <* string "A"

