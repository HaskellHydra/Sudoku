module Sudoku.Helpers where

import Sudoku.Types
import qualified Text.Read as R
import qualified Data.Text as T

-- *** Start Helper functions ***

-- Helper function to convert string to [Locx]
-- *Main> tsplit "_,3,_,4,_"
-- [Loc 0 [],Loc 3 [],Loc 0 [],Loc 4 [],Loc 0 []]
str2Locx :: String -> [Locx]
str2Locx s = (\x -> if x == "_" then Loc 0 [] else Loc (R.read x::Int) [] ) . T.unpack <$> T.splitOn ( T.pack "," ) (T.pack s)
-- Functor Law g <$> f <$> ~ g . f <$>

-- Helper function to convert list of strings to 'list' of 'Locx'
-- convStr2Arr  ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"]
convStr2Arr :: [String] -> [[Locx]]
convStr2Arr  = fmap str2Locx

-- Helper function to extract the list of possible values from list of Locx
-- *Main> g = [Loc 0 [[4,3]],Loc 3 [],Loc 0 [],Loc 4 [],Loc (0::Int) ([[1,2]]::[[Int]])]
-- *Main> getPredictions g
-- [4,3,1,2]
getPredictions :: [Locx] -> [Int]
getPredictions lxs = do
                       Loc _ xs <- lxs
                       foldr (:) [] (concat xs)


-- Helper function to extract the value for a given location
-- *Main> g
-- [Loc 0 [[4,3]],Loc 3 [],Loc 0 [],Loc 4 [],Loc 0 [[1,2]]]
-- *Main> getInts g
-- [0,3,0,4,0]
-- apply getInts <$> grid to get the whole grid ---> [[Int]]
getInts:: [Locx] -> [Int] 
getInts lxs = do
                Loc x _ <- lxs
                foldr (:) [] [x]

-- .---------------.
-- | 2 | 3 | 1 | 4 | ---> R1
-- *---------------* 
-- | 1 | 4 | 2 | 3 | ---> R2
-- *---------------* 
-- | 3 | 2 | 4 | 1 | ---> R3
-- *---------------* 
-- | 4 | 1 | 3 | 2 | ---> R4
-- *---------------*

-- .---------.
-- | p1 | p2 |   
-- *----------   The entier puzzle can be thought 4 quadrants.
-- | p3 | p4 |   Each quadrant is a 2x2 matrix since it is 4x4 puzzle  
-- *---------*
-- Since the puzzle is stored as list of rows to extract the numbers in a
--  quadrant we need 2 rows. 
-- In the above example p1 = [2,3,1,4]
--                      p2 = [1,4,2,3]
-- getQuad will take R1, R2 and generate the quadrants p1,p2

getQuad :: [Int] -> [Int] -> Int -> ([Int], [Int])
getQuad x1 x2 n = let p1 = tList x1 n
                      p2 = tList x2 n in
                  repLists (p1,p2)

-- Helper function to split a even numbered list into list of lists depending on 
--  divison parameter
-- *Main> tList [1..4] 2
-- [[1,2],[3,4]]
tList :: [Int] -> Int -> [[Int]]
tList [] _ = []
tList xs d = take d xs : tList (drop d xs) d

-- repLists is a helper function to getQuad
-- This will swap the 2nd element in the 1st list with the 1st element in the 2nd list
-- *Main> t1 = tList [1..4] 2
-- [[1,2],[3,4]]
-- *Main> t2 = tList [5..8] 2
-- [[5,6],[7,8]]
-- *Main> repLists (t1,t2)
-- ([1,2,5,6],[3,4,7,8])
repLists :: ([[Int]], [[Int]]) -> ([Int], [Int])
repLists p | (x1,x2) <- p = let p1 = head (take 1 x1)
                                p2 = head (take 1 $ drop 1 x1)  in
                                ( concat (p1:take 1 x2) , concat (p2: take 1 (drop 1 x2)) )

-- Helper function to transpose the matrix
getTranspose :: [[Int]] ->  [[Int]]
getTranspose [[],[],[],[]] = []
getTranspose [ p1:p1s, p2:p2s , p3:p3s , p4:p4s] = [p1, p2, p3, p4] : getTranspose [p1s,p2s,p3s,p4s]


generateSeq :: Int -> Int -> [[Int]]
generateSeq x dim = replicate dim x : generateSeq (x+1) dim


-- .---------------. 
-- | 1 | 1 | 2 | 2 |
-- *---------------* 
-- | 1 | 1 | 2 | 2 |
-- *---------------* 
-- | 3 | 3 | 4 | 4 |
-- *---------------* 
-- | 3 | 3 | 4 | 4 |
-- *---------------*
-- generateQuad is used to generate a quadrant map
-- this is used to identify the quadrant for a given location
generateQuad :: Int -> Int -> [[Int]]
generateQuad dim quad = let q = take dim $ generateSeq 1 dim
                            (p1,p2) = repLists ( tList (head q) quad, tList (q!!1) quad )
                            (p3,p4) = repLists (tList (q!!2) quad, tList (q!!3) quad)  in
                        [p1,p2,p3,p4]

-- This is wrapper for getting the quadrants from the grid.
-- Only the location values in the grid are supplied to the below function 
getQuadrant :: [[Int]] -> Int -> [Int]
getQuadrant lxs q =
                  let (q1, q2) = repLists ( tList (head $ take 1 lxs) 2, tList (head $ take 1 $ drop 1 lxs) 2)
                      (q3, q4) = repLists ( tList (head $ take 1 $ drop 2 lxs) 2, tList (head $ take 1 $ drop 3 lxs) 2) in
                  case q of
                    1 -> q1
                    2 -> q2
                    3 -> q3
                    4 -> q4
                    _ -> []

-- Helper function to find the missing numbers between 1-4 in a given list
-- *Main> findMissingElems [2,3,0,0]
-- [1,4]
findMissingElems :: [Int] -> [Int]
findMissingElems xs = filter (/= 0) $ (\ys -> foldr ((:) . (\x -> if x `notElem` ys then x else 0 )) [] [1..4] ) xs

-- This is the core function in solving the puzzle
-- This function will take a list of possible elements in a row, 
--                           list of possible elements along the column 
--                           list of possible elements in a quadrant 
-- And it will find the list of common values
-- If there is only one number it will be the missing value for the location
--  If not we need to run another iteration
getPossibleElems :: [Int] -> [Int] ->[Int] ->[Int]
getPossibleElems ys1 ys2 ys3 = filter (/= 0) $ foldr ((:) . (\x -> if x `elem` ys1 && x `elem` ys2 && x `elem` ys3 then x else 0 )) [] [1..4]

-- Helper function to replace a Locx with a given Locx in a [Locx]
-- The below case will take care of all the pattern matching
-- Combine idx with the value in the list to form a [(idx, x)]
replaceLocElem :: [Locx] -> Locx -> Int -> [Locx]
replaceLocElem xs eLoc@(Loc e _) pos = let d = zip [0..(length xs - 1)] xs in
                                       (\(x,y)-> if x == pos then eLoc else y ) <$> foldr (:) [] d

-- -- Older implementation
-- replaceLocElem :: [Locx] -> Locx -> Int -> [Locx]
-- replaceLocElem [] _ _ = []
-- replaceLocElem xs eLoc@(Loc e _) pos
--   | pos < 0 || pos >= length xs   = xs
--   | otherwise = take pos xs ++ (eLoc : drop (pos+1) xs)

-- Helper function to replace a [Locx] with a given [Locx] in a [[Locx]]
replaceLocList :: [[Locx]] -> [Locx] -> Int -> [[Locx]]
replaceLocList lxs xs pos = let d = zip [0..(length lxs - 1)] lxs in
                                       (\(x,y)-> if x == pos then xs else y ) <$> foldr (:) [] d

-- -- Older implementation
-- replaceLocList :: [[Locx]] -> [Locx] -> Int -> [[Locx]]
-- replaceLocList [] _ _ = []
-- replaceLocList lxs xs pos
--   | pos < 0 || pos >= length lxs = lxs
--   | otherwise = take pos lxs ++ (xs : drop (pos+1) lxs)


printArgs:: [String] -> IO ()
printArgs = foldr ((>>) . putStrLn) (putStrLn "")

-- *** End Helper functions ***
