-- | Programming Paradigms Fall 2022 — Problem Sets 6
-- | Name Surname: Vagif Khalilov
-- | Email: v.khalilov@innopolis.university
-- | Group: BS20-SD-01

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}


import Text.Read (readMaybe, parens)
import Data.Char (toUpper)
import Control.Concurrent.STM (check)
import Distribution.InstalledPackageInfo (InstalledPackageInfo(indefinite))

-- ### EX. 1 ### --

guess :: (b -> Bool) -> (String -> IO b) -> IO b
guess p g = do
    s <- getLine
    x <- g s
    case p x of
        True -> return x
        False -> guess p g

-- Firstly, let's define the type of the p function:
--    we have 'case p x of' - that means p takes an argument of type b and returns True or False, so
--    p :: b -> Bool

-- g - s another function that takes argument of type String (because getLine returns a String) and returns IO b

-- in True case we return x, which is of type IO b, so we get:
-- guess :: (b -> Bool) -> (String -> IO b) -> IO b

-- ### EX. 2 ### --

echo :: IO ()
echo = do
    s <-getLine
    putStrLn (map toUpper s)
    echo

-- ### EX. 3 ### --

-- * 3.a * --

foreverIO :: IO a -> IO b
foreverIO program = do
    program
    foreverIO program

sample3a :: IO b
sample3a = foreverIO (putStrLn "Hello!")

-- * 3.b * --

whenIO :: Bool -> IO () -> IO ()
whenIO cond program = do
    case cond of 
        True -> program 
        False -> return ()

-- * 3.c * --

maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO program = do
    case program of
        Just p -> do
            res <- p 
            return (Just res)
        Nothing -> return Nothing         

-- * 3.d * --

sequenceMaybeIO :: [IO (Maybe a)] -> IO [a] 
sequenceMaybeIO programs = helper programs []
    where 
        helper :: [IO (Maybe a)] -> [a] -> IO [a]
        helper [] res = return res
        helper (p:programs) res = do
            r <- p
            case r of
                Just x -> helper programs (x:res)
                Nothing -> helper programs res

-- * 3.e * --

whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO func l = do
    res <- func l
    case res of
        Just x -> whileJustIO func x 
        Nothing -> return ()

-- * 3.f * --

verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
    putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
    return (x:xs)


forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s 
forStateIO_ el [l] func = do
    func l el
forStateIO_ el (l:list) func = do
     r <- func l el
     forStateIO_ r list func

sample3f = forStateIO_ [] [1, 2, 3] verboseCons


-- ### EX. 4 ### --

example :: IO ()
example = do
    iforIO_ [1, 2] (\i n ->
        iforIO_ "ab" (\j c ->
            print ((i, j), replicate n c)))


iforIO_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_ list func = helper list 0 func 
    where
        helper :: [a] -> Int -> (Int -> a -> IO ()) -> IO () 
        helper [l] ind func = do 
            func ind l
        helper (l:list) ind func = do
            func ind l
            helper list (ind+1) func

--((0,0),"a")
--((0,1),"b")
--((1,0),"aa")
--((1,1),"bb")
