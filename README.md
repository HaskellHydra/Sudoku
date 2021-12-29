# Sudoku
Sudoku - Haskell final project 

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

# **do** syntax refernce

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
