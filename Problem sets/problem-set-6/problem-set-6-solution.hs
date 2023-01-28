-- | link to codeworld - https://code.world/haskell#Ps9bpmF6NCv2lz6irbszlDg
-- | Programming Paradigms Fall 2022 — Problem Sets 6
-- | Name Surname: Vagif Khalilov
-- | Email: v.khalilov@innopolis.university
-- | Group: BS20-SD-01
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade
data Result a   
  = Success a 
  | Failure String 
  
dup f x = f x x
dip f x = f (f x x)
twice f x = f (f x)

-- ### EX. 1 ### --


-- Firstly, let's introduce polymorphic types of 'dup', 'dip' and 'twice':
--     dup   :: (a -> a -> b) -> a -> b
--     dip   :: (a -> a -> a) -> a -> a -> a
--     twice :: (a -> a) -> a -> a

-- * 1.a * --

--     dip (+) 1 2

--     dip :: (a -> a -> a) -> a -> a -> a
--     (+) :: Int -> Int -> Int
--     1   :: Int
--     2   :: Int

--     Int -> Int -> Int = (a -> a -> a)
--     Int = a
--     Int = a

--     Then
--     dip (+) 1 2 :: Int
     
-- * 1.b * --

--     dup (dip (+)) 1

--     dup :: (a -> a -> b) -> a -> b    
--     dip :: (t -> t -> t) -> t -> t -> t
--     (+) :: Int -> Int -> Int
--     1   :: Int
--     for dip (+)  
--         Int -> Int -> Int = (t -> t -> t)
--         So
--         (dip (+)) :: (Int -> Int -> Int)

--     (Int -> Int -> Int) = (a -> a -> b)
--     Int = a
--     So, Int = b
--     dup (dip (+)) 1 :: Int


-- * 1.c * --

--    twice dip

--    twice :: (a -> a) -> a -> a
--    dip   :: (t -> t -> t) -> t -> t -> t 

--    (t -> t -> t) = a
--    t -> t -> t = a

--    So, twice dip :: (t -> t -> t) -> (t -> t -> t)  

-- * 1.d * --

--    dip dip

--    dip :: (a -> a -> a) -> a -> a -> a 
--    dip :: (t -> t -> t) -> t -> t -> t 

--    (t -> t -> t) = a
--    t -> t = a
--    t = a
--    So, we get type error due to infinity type a

-- * 1.e * --

--    twice twice twice

--    twice :: (a -> a) -> a -> a
--    twice :: (b -> b) -> b -> b
--    twice :: (c -> c) -> c -> c

--    for twice twice 
--        (b -> b) = a
--         b -> b  = a
--    twice twice :: (b -> b) -> (b -> b)
    
--    Finally,
--    c -> c = b
--    So, twice twice twice :: (c -> c) -> (c -> c)

-- * 1.f * --

--    dup twice

--    dup   :: (a -> a -> b) -> a -> b
--    twice :: (c -> c) -> c -> c

--    (c -> c) = a
--    c = a
--    c = b
--    So, we get type error due to infinity type a

-- ### EX. 2 ### --

studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA ((Student name A):students) =   name:(studentsWithA students)
studentsWithA (st:students) = studentsWithA students

-- ### EX. 3 ### --

-- * 3.a * --

whileSuccess :: (a -> Result a) -> a -> a
whileSuccess f x = helper (f x) f x 
  where 
    helper ::  (Result a) -> (a -> Result a) -> a -> a
    helper (Failure s) f lx = lx
    helper (Success x) f lx = helper (f x) f x  

example1 = whileSuccess f 1
  where f n | n > 100 = Failure "input is too large" 
                        | otherwise = Success (2 * n)
-- * 3.b * --

applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success f) (Success x) = Success (f x) 
applyResult (Failure s) b = Failure s
applyResult b (Failure s) = Failure s

-- * 3.c * --

fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult succesFunc failFunc (Success a) = (succesFunc a)
fromResult succesFunc failFunc (Failure s) = (failFunc s)

-- * 3.d * --

combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith f (Success a) (Success b) = Success (f a b)
combineResultsWith f (Failure s1) res = Failure s1
combineResultsWith f res (Failure s2)  = Failure s2


-- TEST --

main :: IO()
main = do 
print(studentsWithA [Student "Jack" D, Student "Jane" A])
print(example1)
-- print(applyResult (Success length) (Success [1, 2, 3])) 
-- print(applyResult (Failure "no function") (Failure "no arg"))
print (fromResult (+1) length (Success 3))
print (fromResult (+1) length (Failure "not a number"))
-- print (combineResultsWith (+) (Success 2) (Success 3))
-- print (combineResultsWith (+) (Failure "x is undefined") (Failure "crash"))