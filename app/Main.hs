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
import qualified Text.Read as R
import qualified Data.Text as T
import Data.List


-- import Data.Maybe (Maybe(Nothing))


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

-- The stack for computation --TODO :: Create a State flag for initialization
type App a = WriterT String (StateT (Grid, Coor, PrevVal, Solved, Init) (ReaderT Env IO)) a

runApp :: App ()
runApp = do
           initialize
           (lxs, coor, prevInit, solved, init) <- lift get
           env@(Env dim quad n_x n_y) <- lift $ lift ask
           case coor of
              [] -> do
                checkIfSolved
                (_, _, _, solved, _) <- lift get

                if not solved then (do
                  tell $ "\n\n"++ concat (replicate 50 "*") ++"\n** Running a new iteration\n"++ concat (replicate 50 "*") ++ "\n"
                  runApp
                  ) else (do return ())

              (locxy:locxs) -> do

                        getPossibleValues -- the current App state, env, writer will be passed to the getPossibleValues App function
                        writeIterLog

                        (lxs', coor', prev, _, _) <- lift get
                        lift $ put (lxs', drop 1 coor', prev, False, init) -- move to next location, pop the head of coor

                        runApp -- iterate again

initialize:: App ()
initialize = do
               (lxs, coor, prevInit, solved, init) <- lift get
               env@(Env dim quad n_x n_y) <- lift $ lift ask

               if init then (do
                         liftIO clear
                         tell $ "\n\n"++ concat (replicate 50 "*") ++"\n** Starting Sudoku Solver\n"++ concat (replicate 50 "*") ++ "\n\n"
                         drawPuzzle $ head dim
                         tell $ "\n\n" ++ concat (replicate 50 "*") ++ "\n\n") else (do
                 return ())

               lift $ put (lxs, coor, prevInit, solved, False) -- move to next location, pop the head of coor



checkIfSolved:: App()
checkIfSolved = do
                  (lxs, coor, prevInit, solved, init) <- lift get
                  let check = length $ concat (getPredictions <$> lxs) in
                    lift $ put (lxs, (,) <$> [0..3] <*> [0..3], prevInit, check== 0, init)

                  return ()

writeIterLog :: App()
writeIterLog = do
                 (lxs', coor', prev, _, _) <- lift get
                 env@(Env dim quad n_x n_y) <- lift $ lift ask

                 let (x,y) = head coor'
                     eLoc@(Loc e xs) = lxs' !! x !! y
                     (prevVal, (prevX, prevY)) = prev
                     in
                   if prevVal == 0 && prevVal /= e
                     then
                       tell $ "Found value for the location - (" ++ show x ++", "++ show y ++ "), new Value = " ++ show e ++ "\n"
                     else
                       when ((e == 0) && (length (head xs) > 1)) $
                         tell $ "Found multiple predictions for the location - (" ++ show x ++", "++ show y ++ "), predicted values = " ++ show xs++ "\n" -- tell $ "Value at loc - (" ++ show x ++", "++ show y ++ ")" ++ " is " ++ show e++ "\n"

                 -- TODO : reduce this redundancy
                 let (x,y) = head coor'
                     eLoc@(Loc e xs) = lxs' !! x !! y
                     (prevVal, (prevX, prevY)) = prev
                     in
                   when (prevVal == 0 && prevVal /= e) $
                     drawPuzzle $ head dim

                 return ()

-- get the list of possible values for each (x,y) location in the grid
-- We always pop the head of the coor list we dont need to pass the (x,y) locations
getPossibleValues :: App ()
getPossibleValues = do
                        (lxs, coor, prevInit, solved, init) <- lift get
                        -- liftIO $ putStrLn $ "Solving for location - " ++ (\(x,y) -> "("++show x++","++ show y++")") (head coor)
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
                              lift $ put (replaceLocList lxs (replaceLocElem (lxs !! x) (Loc e [predElems]) y ) x, coor, prevVal, False, init)
                          else
                            if e == 0 && length predElems == 1
                              then
                                lift $ put (replaceLocList lxs (replaceLocElem (lxs !! x) (Loc (head predElems) []) y ) x, coor, prevVal, False, init)
                              else
                                lift $ put (lxs, coor, prevVal, False, init)

                        return  ()

drawPuzzle :: Int -> App ()
drawPuzzle x = do
               (lxs, coor, prev, _, _) <- lift get
               env@(Env dim quad n_x n_y) <- lift $ lift ask

               unless (x/=4)$
                 tell $ "\t\t." ++ concat (replicate 15 "-") ++ ".\n"

               case x of
                 0 ->
                   return ()
                 _ -> do
                   tell $ "\t\t| " ++ T.unpack ( T.replace (T.pack "0") (T.pack "X") (T.pack $ concat (intersperse " | " (show <$> (getInts $ lxs!!(4-x) )  ))) ) ++ " |\n" ++ "\t\t*" ++ concat (replicate 15 "-") ++ "*\n"
                   drawPuzzle $ x - 1

-- ==========================================================================
-- For Manual testing
testGrid :: Grid
testGrid = convStr2Arr  ["_,_,4,_","1,_,_,_","_,2,_,_","_,_,_,3"] -- difficult
-- testGrid = convStr2Arr  ["_,3,4,_","4,_,_,2","1,_,_,3","_,2,1,_"] -- easy
  -- [[Loc 0 [],Loc 3 [],Loc 4 [],Loc 0 []],[Loc 4 [],Loc 0 [],Loc 0 [],Loc 2 []],[Loc 1 [],Loc 0 [],Loc 0 [],Loc 3 []],[Loc 0 [],Loc 2 [],Loc 1 [],Loc 0 []]]

appWithoutWriter :: StateT (Grid, Coor, PrevVal, Bool, Bool) (ReaderT Env IO) ((), String)
appWithoutWriter = runWriterT runApp

appWithoutState :: ReaderT Env IO (((), String), (Grid, Coor, PrevVal, Bool, Bool))
appWithoutState = let coor = (,) <$> [0..3] <*> [0..3] in
                    runStateT appWithoutWriter (testGrid, coor, (0,(0,0)), False, True) -- initial prevVal will be overwritten in the 1st run

appWithoutReader :: IO (((), String), (Grid, Coor, PrevVal, Bool, Bool))
-- appWithoutReader :: App ()
appWithoutReader = runReaderT appWithoutState (Env [4,4] [2,2] 2 2)

-- ==========================================================================

launchApp :: (Maybe Env, Maybe Grid)-> IO (((), String), (Grid, Coor, PrevVal, Bool, Bool))
launchApp (Just env, Just grid) = let coor = (,) <$> [0..3] <*> [0..3] in runReaderT (runStateT (runWriterT runApp) (grid, coor, (0,(0,0)), False, True)) env
              -- case z of
              --   (Just env, Just grid) ->
              --     let coor = (,) <$> [0..3] <*> [0..3] in runReaderT (runStateT (runWriterT runApp) (grid, coor, (0,(0,0)), False, True)) env
              --   (Just env, _) -> print ("Parsed only the Env: " ++ show env) >> return (((), ""), ([], [], (0,(0,0)), False, True))
              --   _ ->  print "parser failed!"  >> return (((), ""), ([], [], (0,(0,0)), False, True))

cli :: IO [String]
cli = getArgs

main :: IO ()
main = do
         args <- cli
        --  printArgs args -- For debug purpose 
         s <- parseFile $ head args
         print s
         z <- convStr2VD s
         check <- checkParser z -- TODO : Put to condition to halt the execution
         if check then (do
                  y <- launchApp z
                  writeFile "dump.txt" $ snd $ fst y
                  putStrLn $ snd $ fst y) else putStrLn "\n\nHalting ....\n\n"


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