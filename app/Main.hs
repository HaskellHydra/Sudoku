module Main where
-- import Sudoku.CLI (cli)
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import System.Environment
import Control.Monad.IO.Class
import Control.Monad
import qualified Text.Read as R
import qualified Data.Text as T
import Data.Maybe (Maybe(Nothing))

-- The entire puzzle is stored as list of rows.
-- Each row is a list of 4 numbers
-- .---------------.
-- | 2 | 3 | 1 | 4 | ---> R1
-- *---------------* 
-- | 1 | 4 | 2 | 3 | ---> R2
-- *---------------* 
-- | 3 | 2 | 4 | 1 | ---> R3
-- *---------------* 
-- | 4 | 1 | 3 | 2 | ---> R4
-- *---------------*

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

-- The stack for computation
type App a = WriterT String (StateT (Grid, Coor, PrevVal, Solved) (ReaderT Env IO)) a

runApp :: App ()
runApp = do
           (lxs, coor, prevInit, solved) <- lift get
           case coor of
              [] -> do
                checkIfSolved
                (_, _, _, solved) <- lift get
                unless solved            -- If solved return else runApp. Then enxt statement 'runApp' is controlled by unless
                  runApp                 -- If it is not yet solved rerun the App and reset all the co ordinates

              (locxy:locxs) -> do

                        getPossibleValues -- the current App state, env, writer will be passed to the getPossibleValues App function
                        writeIterLog

                        (lxs', coor', prev, _) <- lift get
                        lift $ put (lxs', drop 1 coor', prev, False) -- move to next location, pop the head of coor

                        runApp -- iterate again

checkIfSolved:: App()
checkIfSolved = do
                  (lxs, coor, prevInit, solved) <- lift get
                  let check = length $ concat (getPredictions <$> lxs) in
                    lift $ put (lxs, (,) <$> [0..3] <*> [0..3], prevInit, check== 0)

                  return ()

writeIterLog :: App()
writeIterLog = do
                 (lxs', coor', prev, _) <- lift get
                 let (x,y) = head coor'
                     eLoc@(Loc e xs) = lxs' !! x !! y
                     (prevVal, (prevX, prevY)) = prev
                     in
                   if prevVal == 0 && prevVal /= e
                     then
                       tell $ "Found value for the location - (" ++ show x ++", "++ show y ++ "), new Value = " ++ show e ++ "\n"
                     else
                       if (e == 0) && (length (head xs) > 1) then
                         tell $ "Found multiple predictions for the location - (" ++ show x ++", "++ show y ++ "), predicted values = " ++ show xs++ "\n"
                       else
                         tell $ "Value at loc - (" ++ show x ++", "++ show y ++ ")" ++ " is " ++ show e++ "\n"

                 return ()

-- get the list of possible values for each (x,y) location in the grid
-- We always pop the head of the coor list we dont need to pass the (x,y) locations
getPossibleValues :: App ()
getPossibleValues = do
                        (lxs, coor, prevInit, solved) <- lift get
                        liftIO $ putStrLn $ "Solving for location - " ++ (\(x,y) -> "("++show x++","++ show y++")") (head coor)
                        let (x,y) = head coor
                            g = getInts <$> lxs
                            gT = getTranspose g
                            qMap = generateQuad 4 2
                            getQs = getQuadrant g (qMap !! x !! y)
                            possibleElems = [findMissingElems $ g !! x, findMissingElems $ gT !! y, findMissingElems getQs]
                            predElems = if length possibleElems == 3
                                          then
                                            getPossibleElems  (head possibleElems) (possibleElems !! 1) (possibleElems !! 2)
                                           else []
                            -- (findMissingElems $ g !! x) (findMissingElems $ gT !! y) (findMissingElems getQs) 
                            eLoc@(Loc e xs) = lxs !! x !! y
                            prevVal = (e, (x,y)) in

                          if e == 0 && length predElems > 1
                            then
                              lift $ put (replaceLocList lxs (replaceLocElem (lxs !! x) (Loc e [predElems]) y ) x, coor, prevVal, False)
                          else
                            if e == 0 && length predElems == 1
                              then
                                lift $ put (replaceLocList lxs (replaceLocElem (lxs !! x) (Loc (head predElems) []) y ) x, coor, prevVal, False)
                              else
                                lift $ put (lxs, coor, prevVal, False)

                        return  ()

-- ==========================================================================
-- For Manual testing
testGrid :: Grid
testGrid = convStr2Arr  ["_,_,4,_","1,_,_,_","_,2,_,_","_,_,_,3"] -- difficult
-- testGrid = convStr2Arr  ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"] -- easy
  -- [[Loc 0 [],Loc 3 [],Loc 4 [],Loc 0 []],[Loc 4 [],Loc 0 [],Loc 0 [],Loc 2 []],[Loc 1 [],Loc 0 [],Loc 0 [],Loc 3 []],[Loc 0 [],Loc 2 [],Loc 1 [],Loc 0 []]]

appWithoutWriter :: StateT (Grid, Coor, PrevVal, Bool) (ReaderT Env IO) ((), String)
appWithoutWriter = runWriterT runApp

appWithoutState :: ReaderT Env IO (((), String), (Grid, Coor, PrevVal, Bool))
appWithoutState = let coor = (,) <$> [0..3] <*> [0..3] in
                    runStateT appWithoutWriter (testGrid, coor, (0,(0,0)), False) -- initial prevVal will be overwritten in the 1st run

appWithoutReader :: IO (((), String), (Grid, Coor, PrevVal, Bool))
appWithoutReader = runReaderT appWithoutState (Env [4,4] [2,2] 2 2)

-- ==========================================================================


-- pack will convert String to Text
-- Prelude Data.Text> :t splitOn 
-- splitOn :: Text -> Text -> [Text]
-- Use "unpack" to convert back to String
parseFile :: String -> IO ()
parseFile path = do
                   s <- readFile path
                   let str = T.splitOn (T.pack "\n") (T.pack s)
                       cleanStr = take (length s - 1)  s in
                    print $ T.splitOn (T.pack "\n") (T.pack cleanStr)

-- TODO: Create a function to convert the ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"] to proper 'Grid' type
-- s = ["DIM=4x4","QUAD=2x2","_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"]
-- convStr2VD :: [String] -> (Maybe Env, Maybe Grid)
-- convStr2VD (d:q:xs) =
--                   -- TODO: remove the regex redundancy 
--                   let dimRegex = "^DIM=[0-9]+x[0-9]+$"
--                       quadRegex = "^QUAD=[0-9]+x[0-9]+$"
--                   in
--                   if (d =~ dimRegex :: Bool) && (q =~ quadRegex :: Bool) then
--                     let fd = T.splitOn  (T.pack "x") (T.splitOn (T.pack "=") (T.pack d) !! 1)
--                         fq = T.splitOn  (T.pack "x") (T.splitOn (T.pack "=") (T.pack q) !! 1)
--                         dim =  fmap ((\x -> read x::Int ) . T.unpack) fd
--                         quad = fmap ((\x -> read x::Int ) . T.unpack) fq in
--                         (Just $ Env {dim = dim, quad = quad}, Nothing)
--                   else
--                     (Nothing, Nothing)

-- TODO: Mover helper functions into a new file
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

cli :: IO [String]
cli = getArgs

main :: IO ()
main = do
         args <- cli
         printArgs args
         parseFile $ head args
-- Real world usage of uncurry
-- main = cli >>= (uncurry animate)

-- ASCII sudoku art
-- .---------------.
-- | 2 | 3 | 1 | 4 |
-- *---------------*
-- | 1 | 4 | 2 | 3 |
-- *---------------*
-- | 3 | 2 | 4 | 1 |
-- *---------------*
-- | 4 | 1 | 3 | 2 |
-- *---------------*
