module Main where
-- import Sudoku.CLI (cli)
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import System.Environment
-- import System.Random
-- import Text.Regex.TDFA
import qualified Text.Read as R
import qualified Data.Text as T
import Data.Maybe (Maybe(Nothing))

-- Dimension of the puzzle. This is used for Puzzle Env
data Env = Env {dim :: [Int], quad :: [Int], n_x :: Int, n_y ::Int }
                deriving Show

-- (x,y) determine the position
-- 'z' determines the value in the location
type Vector = (Int, Int, Maybe Int)

data Loc a b = Loc a b
           deriving Show

type Locx = Loc Int [[Int]]
-- each quadrant is a 2D grid
type Grid = [[Locx]]

type Coor = [(Int, Int)]

type PrevVal = (Int, (Int, Int))

type Solved = Bool
-- A grid is a group of 2D quadrants 
-- type Grid = [[[[Maybe Int]]]]

-- List of Locations will hold the current state of the puzzle
-- Whatever changes in a app will belong to the State
type Location = Maybe Vector

type App a = WriterT String (StateT (Grid, Coor, PrevVal, Solved) (ReaderT Env IO)) a

-- Prelude> r = (\d -> fmap (\x-> fmap (\y -> y+d) x ) )
---- each quadrant is a 2D grid


-- t = randGen' (1,4) 23
-- p1 = [take 4 t, take 4 $ drop 4 t]
-- p2 = [take 4 $ drop 8 t, take 4 $ drop 12 t]
-- p3 = [take 4 $ drop 16 t, take 4 $ drop 20 t]
-- p4 = [take 4 $ drop 24 t, take 4 $ drop 28 t]
-- q1 = fmap (fmap Just) p1
-- q2 = fmap (fmap Just) p2
-- q3 = fmap (fmap Just) p3
-- q4 = fmap (fmap Just) p4
-- grid = [[q1,q2],[q3,q4]]
-- randGen' :: (Int, Int) -> Int -> [Int]
-- randGen' (x,y) p | (n,_) <- uniformR (x, y) $ mkStdGen $ 137 + p = n : randGen' (x,y) ( 2 * p)
-- Output
-- runReaderT (runStateT (runWriterT runApp) [[[[Just 3,Just 1,Just 1,Just 1],[Just 4,Just 2,Just 2,Just 3]],[[Just 3,Just 4,Just 1,Just 3],[Just 2,Just 4,Just 4,Just 3]]],[[[Just 3,Just 1,Just 2,Just 4],[Just 4,Just 2,Just 1,Just 1]],[[Just 4,Just 2,Just 2,Just 2],[Just 2,Just 3,Just 4,Just 2]]]] )  (Env [4,4] [2,2])
-- (((),""),[[[[Just 3,Just 1,Just 1,Just 1],[Just 4,Just 2,Just 2,Just 3]],[[Just 3,Just 4,Just 1,Just 3],[Just 2,Just 4,Just 4,Just 3]]],[[[Just 3,Just 1,Just 2,Just 4],[Just 4,Just 2,Just 1,Just 1]],[[Just 4,Just 2,Just 2,Just 2],[Just 2,Just 3,Just 4,Just 2]]]])

testGrid :: Grid
-- testGrid = [[Loc 0 [[2,3]],Loc 3 [],Loc 4 [],Loc 0 [[1,2]]],[Loc 1 [],Loc 4 [],Loc 0 [[2,3]],Loc 2 []],[Loc 0 [[3,4]],Loc 2 [],Loc 1 [],Loc 4 []],[Loc 4 [],Loc 1 [],Loc 2 [],Loc 3 []]]
testGrid = convStr2Arr  ["_,_,4,_","1,_,_,_","_,2,_,_","_,_,_,3"] -- difficult
-- testGrid = convStr2Arr  ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"] -- easy

  -- [[Loc 0 [],Loc 3 [],Loc 4 [],Loc 0 []],[Loc 4 [],Loc 0 [],Loc 0 [],Loc 2 []],[Loc 1 [],Loc 0 [],Loc 0 [],Loc 3 []],[Loc 0 [],Loc 2 [],Loc 1 [],Loc 0 []]]

test:: [Int] -> String
test s =
           let f = (1+) in
            case s of
             [] -> ""
             (x:xs) -> show (f x) ++ "--" ++ test (drop 1 s)
          --  test $ drop 1 s

runApp :: App ()
runApp = do
           (lxs, coor, prevInit, solved) <- lift get
           case coor of
              [] -> do
                let check = length $ concat (getPredictions <$> lxs) in
                  lift $ put (lxs, (,) <$> [0..3] <*> [0..3], prevInit, check== 0)
                
                (_, _, _, solved) <- lift get
                if (solved == True ) then
                  return ()
                else
                  runApp -- If it is not yet solved rerun the App and reset all the co ordinates

              (locxy:locxs) -> do
                        let (x,y) = locxy
                            g = getInts <$> lxs
                            gT = getTranspose g
                            qMap = generateQuad 4 2
                            getQs = getQuadrant g (qMap !! x !! y)
                            possibleElems = [findMissingElems $ g !! x, findMissingElems $ gT !! y, findMissingElems getQs]
                            predElems = if length possibleElems == 3 then getPossibleElems  (head possibleElems) (possibleElems !! 1) (possibleElems !! 2)
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
                                  -- tell $ "Value at loc - (" ++ show x ++", "++ show y ++ ")" ++ " is " ++ show e
                                      -- lift $ put (replaceLocList lxs (replaceLocElem (lxs !! x) (Loc e possibleElems) y ) x, coor)

                        (lxs', coor', prev,_) <- lift get
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

                        lift $ put (lxs', drop 1 coor', prev, False) -- move to next location

                        runApp -- iterate again

-- Infinte list of random numbers within a range
-- *Main> t = randGen' (1,4) 23 -- '23' is the seed
-- *Main> take 16 t
-- [3,1,1,1,4,2,2,3,3,4,1,3,2,4,4,3]

appWithoutWriter :: StateT (Grid, Coor, PrevVal, Bool) (ReaderT Env IO) ((), String)
appWithoutWriter = runWriterT runApp

appWithoutState :: ReaderT Env IO (((), String), (Grid, Coor, PrevVal, Bool))
appWithoutState = let coor = (,) <$> [0..3] <*> [0..3] in
                    runStateT appWithoutWriter (testGrid, coor, (0,(0,0)), False) -- initial prevVal will be overwritten in the 1st run

appWithoutReader :: IO (((), String), (Grid, Coor, PrevVal, Bool))
appWithoutReader = runReaderT appWithoutState (Env [4,4] [2,2] 2 2)

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

tsplit :: String -> [Loc Int [[Int]]]
tsplit s = (\x -> if x == "_" then Loc 0 [] else Loc (R.read x::Int) [] ) . T.unpack <$> T.splitOn ( T.pack "," ) (T.pack s)
-- Functor Law g <$> f <$> ~ g . f <$>

-- convStr2Arr  ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"]
convStr2Arr :: [String] -> [[Locx]]
convStr2Arr  = fmap tsplit

getPredictions :: [Locx] -> [Int]
getPredictions lxs = do
                       Loc _ xs <- lxs
                       foldr (:) [] (concat xs)


-- apply getInts <$> grid to get the whole grid ---> [[Int]]
getInts:: [Locx] -> [Int] -- get only the values 
getInts lxs = do
                Loc x _ <- lxs
                foldr (:) [] [x]


getQuad :: [Int] -> [Int] -> Int -> ([Int], [Int])
getQuad x1 x2 n = let p1 = tList x1 n
                      p2 = tList x2 n in
                  repLists (p1,p2)

tList :: [Int] -> Int -> [[Int]]
tList [] _ = []
tList xs d = take d xs : tList (drop d xs) d

repLists :: ([[Int]], [[Int]]) -> ([Int], [Int])
repLists p | (x1,x2) <- p = let p1 = head (take 1 x1)
                                p2 = head (take 1 $ drop 1 x1)  in
                                ( concat (p1:take 1 x2) , concat (p2: take 1 (drop 1 x2)) )

getTranspose :: [[Int]] ->  [[Int]]
getTranspose [[],[],[],[]] = []
getTranspose [ p1:p1s, p2:p2s , p3:p3s , p4:p4s] = [p1, p2, p3, p4] : getTranspose [p1s,p2s,p3s,p4s]


checkElem :: Maybe Int -> [Maybe Int] -> Bool
checkElem elem xs =  or $ (== elem) <$> xs

generateSeq :: Int -> Int -> [[Int]]
generateSeq x dim = replicate dim x : generateSeq (x+1) dim

generateQuad :: Int -> Int -> [[Int]]
generateQuad dim quad = let q = take dim $ generateSeq 1 dim
                            (p1,p2) = repLists ( tList (head q) quad, tList (q!!1) quad )
                            (p3,p4) = repLists (tList (q!!2) quad, tList (q!!3) quad)  in
                        [p1,p2,p3,p4]

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


findMissingElems :: [Int] -> [Int]
findMissingElems xs = filter (/= 0) $ (\ys -> foldr ((:) . (\x -> if x `notElem` ys then x else 0 )) [] [1..4] ) xs

getPossibleElems :: [Int] -> [Int] ->[Int] ->[Int]
getPossibleElems ys1 ys2 ys3 = filter (/= 0) $ foldr ((:) . (\x -> if x `elem` ys1 && x `elem` ys2 && x `elem` ys3 then x else 0 )) [] [1..4]

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

replaceLocList :: [[Locx]] -> [Locx] -> Int -> [[Locx]]
replaceLocList lxs xs pos = let d = zip [0..(length lxs - 1)] lxs in
                                       (\(x,y)-> if x == pos then xs else y ) <$> foldr (:) [] d

-- -- Older implementation
-- replaceLocList :: [[Locx]] -> [Locx] -> Int -> [[Locx]]
-- replaceLocList [] _ _ = []
-- replaceLocList lxs xs pos
--   | pos < 0 || pos >= length lxs = lxs
--   | otherwise = take pos lxs ++ (xs : drop (pos+1) lxs)


-- Create list of possible values (test function) 
getPossibleValues :: (Int, Int) -> [[Locx]] -> [[Locx]]
getPossibleValues (x,y) lxs = let g = getInts <$> lxs
                                  gT = getTranspose g
                                  qMap = generateQuad 4 2
                                  getQs = getQuadrant g (qMap !! x !! y)
                                  possibleElems = [findMissingElems $ g !! x, findMissingElems $ gT !! y, findMissingElems getQs]
                                 --  possibleElems = getPossibleElems (findMissingElems $ g !! x) (findMissingElems $ gT !! y) (findMissingElems getQs) 
                                  eLoc@(Loc e xs) = lxs !! x !! y in
                              if e == 0 then
                                replaceLocList lxs (replaceLocElem (lxs !! x) (Loc e possibleElems) y ) x
                              else
                                 lxs

-- Apply zip to combine idx for values
-- *Main> d = zip [1..4] $ (3*) <$> [1..4]
-- [(1,3),(2,6),(3,9),(4,12)]
-- Apply 'foldr' with fmap to modify the values
-- (\(x,y)-> x+ y) <$>foldr (:) [] d
-- [4,8,12,16]
-- Replace a specific element in the list
-- *Main> (\(x,y)-> if(x ==2) then y*3 else y ) <$>foldr (:) [] d
-- [3,18,9,12]



-- (,) <$> [0..3] <*> [0..3]
-- computeGrid :: (Int, Int) -> [[Int]] -> [Int] 
-- computeGrid (x,y) grid = case (grid !! x) !! y of
--                           Just n -> [n]
--                           Nothing -> []


-- convStr2VD :: [String] -> Maybe Env
-- convStr2VD (x:xs) =
--                   let dimRegex = "^DIM=[0-9]+x[0-9]+$" 
--                       quadRegex = "^QUAD=[0-9]+x[0-9]+$" 
--                   in
--                   if (x =~ dimRegex :: Bool) then
--                     let f = T.splitOn  (T.pack "x") (T.splitOn (T.pack "=") (T.pack x) !! 1)
--                         dim =  fmap ((\x -> read x::Int ) . T.unpack) f in
--                         Just $ Env {dim = dim, quad =[]}
--                   else
--                     Nothing

-- sum [1,2,3] = foldr (+) 0 [1,2,3]
-- [1,2,3] === 1:(2:(3:[]))
-- [1,2,3] === 1+(2+(3:+0))

printArgs:: [String] -> IO ()
printArgs = foldr ((>>) . putStrLn) (putStrLn "")


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
