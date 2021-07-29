{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- Basically a copy of TestText with tests for chunks and no tests for partial
-- input
module TestTextLazy ( tests ) where

import Distribution.TestSuite as TestSuite
import Replace.Attoparsec.Text.Lazy
import Data.Attoparsec.Text.Lazy as A
import Data.Attoparsec.Combinator
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Text.Parser.Char (upper)
import Control.Applicative
import Data.Bifunctor

findAllCap' :: Parser a -> Parser [Either T.Text (TS.Text, a)]
findAllCap' sep = sepCap (match sep)
findAll' :: Parser b -> Parser [Either T.Text TS.Text]
findAll' sep = (fmap.fmap) (second fst) $ sepCap (match sep)

tests :: IO [Test]
tests = return
    [ Test $ runParserTest "findAllCap upperChar"
        (findAllCap' (upper :: Parser Char))
        ("aBcD" :: TS.Text)
        [Left "a", Right ("B", 'B'), Left "c", Right ("D", 'D')]
    -- check that sepCap can progress even when parser consumes nothing
    -- and succeeds.
    , Test $ runParserTest "zero-consumption parser"
        (sepCap (many (upper :: Parser Char)))
        ("aBcD" :: TS.Text)
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
        (findAll' ((A.takeWhile (=='𝅘𝅥𝅯') :: Parser TS.Text)))
        ("𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :: TS.Text)
        [Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥", Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯", Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥"]
    , Test $ runParserTest "empty input" (sepCap (fail "" :: Parser ())) "" []
    , Test $ streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    , Test $ streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    , Test $ streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
    , Test $ streamEditTest "empty input" (match (fail "")) (T.fromStrict . fst) "" ""
    , Test $ breakCapTest "basic" upper "aAa" (Just ("a", 'A', "a"))
    , Test $ breakCapTest "first" upper "Aa" (Just ("", 'A', "a"))
    , Test $ breakCapTest "last" upper "aA" (Just ("a", 'A', ""))
    , Test $ breakCapTest "fail" upper "aaa" Nothing
    , Test $ breakCapTest "match" (match upper) "aAa" (Just ("a", ("A",'A'), "a"))
    , Test $ breakCapTest "zero-width" (lookAhead upper) "aAa" (Just ("a",'A', "Aa"))
    , Test $ breakCapTest "empty input" upper "" Nothing
    , Test $ breakCapTest "empty input zero-width" (return () :: Parser ()) "" (Just ("", (), ""))
    , Test $ chunkTest "all chunks are processed" (string "x") (const "y") ["x", "x", "x"] id "yyy"
    , Test $ chunkTest "matches across chunks" (string "abcd") (const "y") ["ab", "cd", "ab", "cd"] id "yy"
    , Test $ chunkTest "multiple matches in one chunks" (string "x") (const "y") ["xxx", "xx"] id "yyyyy"
    , Test $ chunkTest "chunk processing is lazy" (string "abcd") (const "y") ["ab", "cd", "foo", error "chunk was read unnecessarily"] (T.take 1) "y"
    ]
  where
    chunkTest nam sep editor input post expected = TestInstance
            { run = do
                let output = post $ streamEdit sep editor (T.fromChunks input)
                if (output == expected)
                    then pure (Finished Pass)
                    else return (Finished $ TestSuite.Fail
                        $ "got " <> show (T.toChunks output) <> " expected " <> show (T.toChunks expected))
            , name = "chunkTest " ++ nam
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options supported"
            }

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


