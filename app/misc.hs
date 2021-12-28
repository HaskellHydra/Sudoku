-- import System.Random
-- import Text.Regex.TDFA


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


test:: [Int] -> String
test s =
           let f = (1+) in
            case s of
             [] -> ""
             (x:xs) -> show (f x) ++ "--" ++ test (drop 1 s)
          --  test $ drop 1 s


-- Infinte list of random numbers within a range
-- *Main> t = randGen' (1,4) 23 -- '23' is the seed
-- *Main> take 16 t
-- [3,1,1,1,4,2,2,3,3,4,1,3,2,4,4,3]


-- Create list of possible values (test function) 
-- getPossibleValues :: (Int, Int) -> [[Locx]] -> [[Locx]]
-- getPossibleValues (x,y) lxs = let g = getInts <$> lxs
--                                   gT = getTranspose g
--                                   qMap = generateQuad 4 2
--                                   getQs = getQuadrant g (qMap !! x !! y)
--                                   possibleElems = [findMissingElems $ g !! x, findMissingElems $ gT !! y, findMissingElems getQs]
--                                  --  possibleElems = getPossibleElems (findMissingElems $ g !! x) (findMissingElems $ gT !! y) (findMissingElems getQs) 
--                                   eLoc@(Loc e xs) = lxs !! x !! y in
--                               if e == 0 then
--                                 replaceLocList lxs (replaceLocElem (lxs !! x) (Loc e possibleElems) y ) x
--                               else
--                                  lxs

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
