module Main where

import Data.Maybe (catMaybes)
import MyLib (isSpecialSumSet, optimalSetOfLength, sumString)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main =
  hspec $
    describe
      "Tests of optimalSetOfLength"
      $ do
        it
          "first few are correct"
          $ catMaybes (optimalSetOfLength <$> [1 .. 6])
            `shouldBe` firstFewSpecials
        it
          "isSpecialSumSet works on first few"
          $ all
            isSpecialSumSet
            firstFewSpecials
            `shouldBe` True
        it
          "Negative value for isSpecialSumSet"
          $ isSpecialSumSet [1, 2, 3] `shouldBe` False
        it
          "sumString works for first few values"
          $ sumString
            <$> firstFewSpecials
            `shouldBe` [ "1",
                         "12",
                         "234",
                         "3567",
                         "69111213",
                         "111819202225"
                       ]
  where
    firstFewSpecials :: [[Int]]
    firstFewSpecials =
      [ [1],
        [1, 2],
        [2 .. 4],
        [3, 5, 6, 7],
        [6, 9, 11, 12, 13],
        [11, 18, 19, 20, 22, 25]
      ]