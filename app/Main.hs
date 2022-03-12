module Main where

import Data.Maybe (fromJust)
import MyLib (optimalSetOfLength, sumString)

main :: IO ()
main = do
  putStrLn "The encoding string for the minimum sum special sum set of length 7 is:"
  putStrLn $ sumString $ fromJust $ optimalSetOfLength 7