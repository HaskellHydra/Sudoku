module Sudoku.Types where

-- Dimension of the puzzle. This is used for Puzzle Env
data Env = Env {dim :: [Int], quad :: [Int], n_x :: Int, n_y ::Int }
                deriving Show

data Loc a b = Loc a b
           deriving Show

-- Each location is a value and a list of possible values
type Locx = Loc Int [[Int]]

-- Grid will hold the entire puzzle
type Grid = [[Locx]]

-- This list will contain the caterisian co-ordinates for the possible locations
-- (0,0) -> (3,3) since we are focused only on 4x4 sudoku puzzle
type Coor = [(Int, Int)]

-- This type will store the previous value of a location and the co-ordinates
--  of the location
type PrevVal = (Int, (Int, Int))

-- This type is a flag to indicate if the puzzle has been solved
type Solved = Bool

