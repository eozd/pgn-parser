module Main where

import qualified Data.Map as M
import PgnParser
import PgnParserTypes
import Text.Parsec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Sample test" $ do
    it "sample-1" $ do
      runParser parsePGN () "test" "" `shouldBe` (Right $ ChessGame M.empty [])
    -- it "sample-1" $ do
