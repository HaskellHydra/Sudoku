module Main where

-- TODO: Check the imports after completion

-- import Sudoku.CLI (cli)
import Sudoku.Types
import Sudoku.Helpers
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import System.Environment
import Control.Monad.IO.Class
import Control.Monad
import Text.Regex.TDFA
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
parseFile :: String -> IO [String]
parseFile path = do
                   s <- readFile path
                   let str = T.splitOn (T.pack "\n") (T.pack s)
                       cleanStr = take (length s - 1)  s in
                      -- print cleanStr
                      return $ T.unpack <$> T.splitOn (T.pack "\n") (T.pack cleanStr)

-- TODO: Create a function to convert the ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"] to proper 'Grid' type
-- s = ["DIM=4x4","QUAD=2x2","_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"]
convStr2VD :: [String] -> (Maybe Env, Maybe Grid)
convStr2VD (d:q:xs) =
                  -- TODO: remove the regex redundancy 
                  let dimRegex = "^DIM=[0-9]+x[0-9]+$"
                      quadRegex = "^QUAD=[0-9]+x[0-9]+$"
                  in
                  if (d =~ dimRegex :: Bool) && (q =~ quadRegex :: Bool) then
                    let fd = T.splitOn  (T.pack "x") (T.splitOn (T.pack "=") (T.pack d) !! 1)
                        fq = T.splitOn  (T.pack "x") (T.splitOn (T.pack "=") (T.pack q) !! 1)
                        dim =  fmap ((\x -> read x::Int ) . T.unpack) fd
                        quad = fmap ((\x -> read x::Int ) . T.unpack) fq in
                        (Just $ Env {dim = dim, quad = quad, n_x = head quad, n_y = head quad}, Nothing)
                  else
                    (Nothing, Nothing)

testParse :: [String] -> IO ()
testParse s = case convStr2VD s of
                (Just env, _) -> print $ "Parsed only the ENV: " ++ show env
                _ ->  print "parser failed!"

-- TODO: Mover helper functions into a new file

cli :: IO [String]
cli = getArgs

main :: IO ()
main = do
         args <- cli
         printArgs args
         s <- parseFile $ head args
         testParse s

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
