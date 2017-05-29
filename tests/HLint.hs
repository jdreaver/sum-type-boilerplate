module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

import Prelude (String, IO, null)

arguments :: [String]
arguments =
  [ "library"
  , "tests"
  , "-i=Redundant do"
  ]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints then exitSuccess else exitFailure
