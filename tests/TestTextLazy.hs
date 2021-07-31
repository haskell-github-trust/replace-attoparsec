{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- Basically a copy of TestText with tests for chunks and no tests for partial
-- input
module TestTextLazy ( tests ) where

import Distribution.TestSuite as TestSuite
import Replace.Attoparsec.Text.Lazy
import Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy as T

tests :: IO [Test]
tests = return
    [ Test $ streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    , Test $ streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    , Test $ streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
    , Test $ streamEditTest "empty input" (match (fail "")) fst "" ""
    , Test $ chunkTest "all chunks are processed" (string "x") (const "y") ["x", "x", "x"] id "yyy"
    , Test $ chunkTest "matches across chunks" (string "abcd") (const "y") ["ab", "cd", "ab", "cd"] id "yy"
    , Test $ chunkTest "multiple matches in one chunks" (string "x") (const "y") ["xxx", "xx"] id "yyyyy"
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
