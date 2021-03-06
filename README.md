# Sudoku

* The 4x4 sudoku puzzle is stored as row of lists as follows

```
        .---------------.
        | X | 3 | 4 | X | ---> R1
        *---------------* 
        | 4 | X | X | 2 | ---> R2
        *---------------* 
        | 1 | X | X | 3 | ---> R3
        *---------------* 
        | X | 1 | 2 | X | ---> R4
        *---------------*

```
* The missing locations contain a 0 value.
* For each of the locations the possible numbers along the row, column and the quadrant are analyzed and stored in 3 lists. 
* If there is only one such common number among these 3 lists it is determined as the missing value. 
* If there are multiple possible values for a given location the location will no tbe updated in the current iteration.
* If the puzzle is not solved in a single iteration [a pass through all the locations (0,0) -> (3,3)] the program will initiate another iteration.
* The process is repeated until the puzzle is solved (i.e every location has a value).
* To run the program we just supply a text file with the puzzle.
  * Check the below section to run the program.
  * Check the puzzle.txt for the text format to describe the puzzle.
* The logs are stored in the Writer String and at the end of the program it is written to dump.txt

# Summary

The main program is split into the following function calls

* The monad transformer stack used for the App is as shown below.

```
type App a = WriterT String (StateT (Grid, Coor, PrevVal, Solved, Init) (ReaderT Env IO)) a

```

* launchApp :: (Maybe Env, Maybe Grid)-> IO (((), String), (Grid, Coor, PrevVal, Bool, Bool))
  * The launchApp will encapsulate the runApp function.
  * The parameters are parsed from the cli function and used in the launchApp 
* runApp :: App ()
  * The runApp is the main loop of the program and responsible for sloving the entire puzzle using the below functions
* initialize:: App ()
  * This function is resposible for the intialization of the App and drawing the initial puzzle on to the screen
* checkIfSolved:: App()
  * This function will check if the puzzle is solved by going through all the cells of the puzzle. 
* writeIterLog :: App()
  * This will log all the newly found numbers for a cell and if a cell has 2 possible values.
* getPossibleValues :: App ()
 * This will output expected number for a empty cell by checking the coressponding row, column and the quadrant.
* drawPuzzle :: Int -> App ()
  * This function will draw the puzzle on to the screen at any given state.
* The Helper.hs holds all the functions used for the cell computation and the Types.hs contains all the types used by the App.
* File structure

```
????????? app
??????? ????????? Main.hs
??????? ????????? misc.hs
????????? CHANGELOG.md
????????? dump.txt
????????? FinalProject.cabal
????????? puzzle-d1.txt
????????? puzzle-d.txt
????????? puzzle.txt
????????? README.md
????????? src
    ????????? Sudoku
        ????????? Helpers.hs
        ????????? Types.hs
```

## Future improvements

* Use **hmatrix** library to store the puzzle instead of lists. It will be easy to perform matrix operations.
* Make this program more generalized to solve sudoku puzzles of different dimensions.
* In the current implementation the possible values from previous iterations are ignored in the next iteration.
  * In the case of 4x4 puzzle it may work but for 8x8 puzzle this program will fail.
* Create a batch mode to process multiple puzzles from a single file.

# Run 

```
cabal run sudoku-cli ./puzzle.txt
```

# Cabal commands

```
cabal install --lib --package-env . regex-tdfa random lens transformers
```

```
cabal repl FinalProject.cabal
```

```
cabal repl FinalProject.cabal
 > :l app/Main.hs
```

* Run the following command in the interactive cabal repl to test regex

```
:set -XFlexibleContexts
```

# **do** syntax reference

* Conditional **do** using **case**

```
    case init of
       True ->  do
                 liftIO clear
                 tell $ "\n\n"++ concat (replicate 50 "*") ++"\n** Starting Sudoku Solver\n"++ concat (replicate 50 "*") ++ "\n\n"
                 drawPuzzle $ head dim
                 tell $ "\n\n" ++ concat (replicate 50 "*") ++ "\n\n"
       _ -> do
         return ()
```

* Conditional **do** using **if**

```
    if check then (do
             y <- launchApp z
             writeFile "dump.txt" $ snd $ fst y
             putStrLn $ snd $ fst y) else putStrLn "\n\nHalting ....\n\n"

```

* Conditional **do** using **unless** or **when** (Use this when there is only one condition to check)

```
    when (prevVal == 0 && prevVal /= e) $
      drawPuzzle $ head dim
```
