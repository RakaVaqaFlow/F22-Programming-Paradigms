-- | link to codeworld - https://code.world/haskell#PUhjs7A9xaoDhAenGmtlCtQ
-- | Programming Paradigms Fall 2022 — Problem Sets 5
-- | Name Surname: Vagif Khalilov
-- | Email: v.khalilov@innopolis.university
-- | Group: BS20-SD-01

type Bit = Int

-- # Ex. 1 # --

-- * 1.a * --

binaryToDecimal :: [Bit] -> Int
binaryToDecimal bits = binaryToDecimalReversed  (reverse bits)
  where
    binaryToDecimalReversed :: [Bit] -> Int
    binaryToDecimalReversed [] = 0
    binaryToDecimalReversed (1:bits) = 1 + 2 * (binaryToDecimalReversed bits)
    binaryToDecimalReversed (0:bits) = 2 * (binaryToDecimalReversed bits)
    
-- * 1.b * --

countZeros :: [Bit] -> Int
countZeros [] = 0
countZeros (0:bits) = countZeros bits
countZeros (1:bits) = countAllZeros bits
  where
    countAllZeros :: [Bit] -> Int
    countAllZeros [] = 0
    countAllZeros (1:bits) = countAllZeros(bits)
    countAllZeros (0:bits) = 1 + countAllZeros(bits)
    
-- * 1.c * --

encodeWithLengths :: [Bit] -> [Bit]
encodeWithLengths [] = []
encodeWithLengths (0:bits) = encodeWithLengths(bits)
encodeWithLengths (b:bits) = encoder b 1 bits
  where 
    encoder :: Bit -> Int -> [Bit] -> [Bit]
    encoder bit cnt [] = [cnt]
    encoder 1 cnt (1:bits) = encoder 1 (cnt+1) bits
    encoder 0 cnt (0:bits) = encoder 0 (cnt+1) bits
    encoder bit cnt (b:bits) =  cnt : (encoder b 1 bits)

-- * 1.d * --

binaryOdd :: [Bit] -> Bool
binaryOdd [] = False
binaryOdd (1:[]) = True
binaryOdd (0:[]) = False
binaryOdd (x:bits) = binaryOdd bits

-- * 1.e * --
decrement :: [Bit] -> [Bit]
decrement bits = reverse (decrementReversed (reverse bits))
  where 
    decrementReversed :: [Bit] -> [Bit]
    decrementReversed [] = []
    decrementReversed (0:[]) = [0]
    decrementReversed (1:[]) = []
    decrementReversed (1:bits) = 0 : bits
    decrementReversed (b:bits) = 1 : (decrementReversed bits)

-- * 1.f * --

propagate :: (Bool, [Int]) -> [(Bool, Int)] 
propagate (bl, []) = []
propagate (bl, (n:nums))= (bl, n) : (propagate (bl, nums))

-- # Ex. 2 # --

-- * 2.a * --

alternatingSum :: [Int] -> Int
alternatingSum [] = 0
alternatingSum (x:[]) = x
alternatingSum (x:(y:nums)) = x - y + (alternatingSum nums)

-- * 2.b * --

-- | EQUATIONAL REASONING | --

--     alternatingSum [1,2,3,4,5]
-- ==> 1 - 2 + (alternatingSum [3, 4, 5])
-- ==> 1 - 2 + (3 - 4 + (alternatingSum 5))
-- ==> 1 - 2 + (3 - 4 + 5)
-- ==> 1 - 2 + 4
-- ==> 3

-- # Ex. 3 # --

data Radians = Radians Double deriving Show
data Degrees = Degrees Double deriving Show
-- | name 'pi' is already reserved 
piNum :: Double
piNum = 3.14159

toDegrees :: Radians -> Degrees
toDegrees (Radians r) = Degrees ((r * 180) / piNum)

fromDegrees :: Degrees -> Radians
fromDegrees (Degrees d) = Radians ((d * piNum) / 180)

-- | to test the function | --

-- * 60 degrees = 1,0472 radians * -- 
deg60 :: Degrees
deg60 = Degrees 60

-- * pi/4 radians = 45 degrees * --
radPiOv4 :: Radians
radPiOv4 = Radians (piNum/4)

-- # OUTPUT # --

main :: IO ()
main = do
print (binaryToDecimal [1,0,1,1,0])
print (countZeros [0,0,0,1,0,1,1,0])
print (encodeWithLengths [0,0,0,1,1,0,1,1,1,0,0])
print (binaryOdd [1,0,1,1,0])
print (binaryOdd [1,0,1,1,1])
print (decrement [1,0,1,1,0])
print (decrement [1,0,0,0,0])
print (decrement [0]) 
print (propagate (False, [1, 2, 3]))
print (propagate (True, [1, 1]))
print (alternatingSum [6,2,4,1,3,9])
print (toDegrees radPiOv4)
print (fromDegrees deg60)