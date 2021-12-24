module Main where
import Sudoku.CLI (cli)

printArgs:: [String] -> IO ()
printArgs xs = foldr ((>>) . putStrLn) (putStrLn "") xs


main :: IO ()
main = cli >>= printArgs
-- Real world usage of uncurry
-- main = cli >>= (uncurry animate)