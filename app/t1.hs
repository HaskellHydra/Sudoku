import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

type App a = WriterT String (StateT Int (Reader Int)) a

-- 'a' is the Writer input
-- Writer w a -> (a,w)

-- computation:: App Int
-- computation = do
--                 x <- lift get
--                 return x

printX:: Int -> IO ()
printX = print

computation:: App Int
computation = do
                x <- lift get -- get state constant
                -- y <- lift $ lift ask
                tell $ "State is " ++ show x
                -- tell $ "State is (" ++ show x ++ "," ++ show y ++ ")"
                -- lift $ put y
                -- let k = print x
                -- return () >> do 
                -- above statement is equivalent to below     

                do 
                y <- lift $ lift ask -- get reader constant
                lift $ put y -- insert the state
                z <- lift get 
                tell $ "\\State is " ++ show z
                -- tell $ "State is (" ++ show x ++ "," ++ show y ++ ")"
                return 1