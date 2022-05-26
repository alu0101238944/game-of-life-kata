{-# LANGUAGE TupleSections #-}

module GameOfLife where

import Data.Matrix

type Position = (Int, Int)
type Grid = Matrix Cell
data Cell = Dead | Alive deriving (Eq)

gameOfLife :: Grid -> Grid
gameOfLife grid = grid

setAlive :: Position -> Grid -> Grid
setAlive = setElem Alive

setDead :: Position -> Grid -> Grid
setDead = setElem Dead

countNeighbours :: Position -> Grid -> Int
countNeighbours (i ,j) matrix = length $ filter (== Alive) neighbours
  where 
    neighbours = map (matrix !) neighboursPositions
    neighboursPositions = map (\(a, b) -> (a + i, b + j)) neighbourOffsets
    neighbourOffsets = filter (/= (0, 0)) $ concatMap (\(i) -> map (, i) neighboursWindow) neighboursWindow
    neighboursWindow = [-1, 0, 1]
