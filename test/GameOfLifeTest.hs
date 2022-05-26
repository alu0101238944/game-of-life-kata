module Main where

import GameOfLife
import Test.Hspec

import Data.Matrix

main :: IO ()
main = hspec gameOfLifeTests

{- 
  GameOfLife
  ==========
    - Count alive neighbours
    - Dead cell with exactly 3 alive neighbours is alive in the next step
    - Alive cell with 2 or 3 alive neighbours is alive in the next step, in other case dies
-}

gameOfLifeTests :: Spec
gameOfLifeTests = describe "GameOfLife behaviour" $ do
  it "Should be posible to count the neighbouring alive cells" $ do
    countNeighbours (3, 3) matrix1 `shouldBe` 8
    countNeighbours (4, 4) matrix1 `shouldBe` 2
    countNeighbours (3, 4) matrix1 `shouldBe` 4

    where matrix1 = fromLists [
                                [Dead, Dead, Dead, Dead, Dead],
                                [Dead, Alive, Alive, Alive, Dead],
                                [Dead, Alive, Dead, Alive, Dead],
                                [Dead, Alive, Alive, Alive, Dead],
                                [Dead, Dead, Dead, Dead, Dead]
                              ] 
