module Main where
-- import Sudoku.CLI (cli)

import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import System.Environment
import qualified Data.Text as T

-- Dimension of the puzzle. This is used for Puzzle Env
newtype Env = Env {dim :: (Int, Int)}
                deriving Show

-- (x,y) determine the position
-- 'z' determines the value in the location
type Vector = (Int, Int, Maybe Int)

-- List of Locations will hold the current state of the puzzle
-- Whatever changes in a app will belong to the State
type Location = Maybe Vector

type App a = WriterT String (StateT [Vector] (ReaderT Env IO)) a


-- runReaderT (runStateT (runWriterT runApp) [(1,2,Just 3), (2,1,Just 6), (1,1, Nothing)])  (Env (4,4))
-- (((),""),[(1,2,Just 3),(2,1,Just 6),(1,1,Nothing)])
runApp :: App ()
runApp = do
           x <- lift $ lift ask
        --    let y = print x 
        --    z <-  lift $ lift y
           return ()



-- pack will convert String to Text
-- Prelude Data.Text> :t splitOn 
-- splitOn :: Text -> Text -> [Text]
-- Use "unpack" to convert back to String
parseFile :: String -> IO ()
parseFile path = do
                   s <- readFile path
                   print $ T.splitOn (T.pack "\n") (T.pack s)


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