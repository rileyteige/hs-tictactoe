module Main where

import TicTacToe
import TicTacToe.Board()
import TicTacToe.Player()

import Test.HUnit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Board" [
     testCase "BoardInitShow" test_board_init_show
    ]
  ]

test_board_init_show = (show initBoard) @?= expectedStr
  where
    expectedStr = unlines [
      "...",
      "...",
      "..."
      ]


