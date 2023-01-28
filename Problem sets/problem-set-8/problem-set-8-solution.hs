-- | Programming Paradigms Fall 2022 — Problem Set 8
-- | Name Surname: Vagif Khalilov
-- | Email: v.khalilov@innopolis.university
-- | Group: BS20-SD-01

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- ### EX. 1 ### --

-- * 1.a * --
isSingleton :: [a] -> Bool
isSingleton [x] = True
isSingleton _ = False

example1_a1 = isSingleton [1] -- => True
example1_a2 = isSingleton [1..] -- => False
example1_a3 = isSingleton [[1..]] -- => True

-- * 1.b * --
insert :: Int -> [Int] -> [Int]
insert el [] = [el]
insert el (x:xs)
    | el <= x = [el, x] ++ xs 
    | el > x = x: insert el xs
    

example1_b1 = insert 3 [1,2,5,7] -- => [1,2,3,5,7]
example1_b2 = insert 3 [0,1,1] -- => [0,1,1,3]
example1_b3 = take 5 (insert 3 [1..]) -- => [1,2,3,3,4]

-- * 1.c * --
separateBy :: a -> [a] -> [a]
separateBy el list = foldr (\ x -> (++) [x, el]) [] list

example1_c1 = separateBy ',' "hello" -- => "h,e,l,l,o"
example1_c2 = take 5 (separateBy 0 [1..]) -- => [1,0,2,0,3]

-- * 1.d * --
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot func lst = (takeWhile func lst, dropWhile func lst)
example1_d1 = splitWhenNot (/= ' ') "Hello, world!" 
-- => ("Hello,"," world!")

example1_d2 = take 10 (fst (splitWhenNot (< 100) [1..])) 
-- => [1,2,3,4,5,6,7,8,9,10]

example1_d3 = take 10 (snd (splitWhenNot (< 100) [1..])) 
-- => [100,101,102,103,104,105,106,107,108,109]

example1_d4 = take 10 (fst (splitWhenNot (> 0) [1..])) 
-- => [1,2,3,4,5,6,7,8,9,10]

-- * 1.e * --
groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy func [] = []
groupsSeparatedBy func lst = helper func lst
    where
    helper func lst = takePart func lst : groupsSeparatedBy func (dropPart func lst)
        where
        takePart func [] = [] 
        takePart func (x:xs) 
            | func x = []
            | otherwise = x : takePart func xs
        dropPart func [] = [] 
        dropPart func (x:xs) 
            | func x = xs
            | otherwise = dropPart func xs
    

example1_e1 = groupsSeparatedBy (== ' ') "Here are some words!"
-- => ["Here","are","some","words!"]

example1_e2 = take 3 (groupsSeparatedBy (\n -> n `mod` 4 == 0) [1..])
-- => [[1,2,3],[5,6,7],[9,10,11]]

-- * 1.f * --
replicateWithPos :: [a] -> [a]
replicateWithPos [] = []
replicateWithPos lst = helper 1 lst
    where 
        helper _ [] = []
        helper n (x:xs) = replicate n x ++ helper (n+1) xs

example1_f1 = replicateWithPos [1..3]
-- => [1,2,2,3,3,3]

example1_f2 = replicateWithPos "Hello"
-- => "Heelllllllooooo"

example1_f3 = take 10 (replicateWithPos [1..])
-- => [1,2,2,3,3,3,4,4,4,4]


-- ### EX. 2 ### --

-- * 2.a * --
lucas :: [Int]
lucas = [2, 1] ++ zipWith (+) lucas (helper lucas)
    where
        helper (x:xs) = xs

example2_a1 = take 10 lucas 
-- => [2,1,3,4,7,11,18,29,47,76]

-- * 2.b * --
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 num = [1] ++ helper 1
    where
        helper x = (x - x/2 + 1/x) : helper (x - x/2 + 1/x)

example2_b1 = take 5 (approximationsOfRoot2 1)
-- => [1.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899]