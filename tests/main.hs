module Main where

import ChessBoard
import ChessBoard.Position
import ChessBoard.Pieces

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe,isNothing,isJust)

import Test.HUnit
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Data.Either.Unwrap (fromRight)

main :: IO ()
main = defaultMain []