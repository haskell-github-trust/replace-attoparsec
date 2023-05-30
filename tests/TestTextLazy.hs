{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

-- Basically a copy of TestText with tests for chunks and no tests for partial
-- input
module TestTextLazy ( tests ) where

import Replace.Attoparsec.Text.Lazy
import Data.Attoparsec.Text.Lazy as A
import qualified Data.Text.Lazy as T
import Test.Hspec (describe, shouldBe, it, SpecWith)

tests :: SpecWith ()
tests = describe "input Text.Lazy" do
    streamEditTest "x to o" (string "x") (const "o") "x x x" "o o o"
    streamEditTest "x to o inner" (string "x") (const "o") " x x x " " o o o "
    streamEditTest "ordering" (string "456") (const "ABC") "123456789" "123ABC789"
    streamEditTest "empty input" (match (fail "")) fst "" ""
    chunkTest "all chunks are processed" (string "x") (const "y") ["x", "x", "x"] id "yyy"
    chunkTest "matches across chunks" (string "abcd") (const "y") ["ab", "cd", "ab", "cd"] id "yy"
    chunkTest "multiple matches in one chunks" (string "x") (const "y") ["xxx", "xx"] id "yyyyy"
  where
    chunkTest nam sep editor input post expected = it nam $ shouldBe
        (post $ streamEdit sep editor (T.fromChunks input)) expected

    streamEditTest nam sep editor input expected = it nam $ shouldBe
        (streamEdit sep editor input) expected
