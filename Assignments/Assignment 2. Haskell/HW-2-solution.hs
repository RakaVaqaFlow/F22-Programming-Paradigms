-- | Programming Paradigms Fall 2022 — Homework Assignment №2
-- | Name Surname: Vagif Khalilov
-- | Email: v.khalilov@innopolis.university
-- | Group: BS20-SD-01

import CodeWorld


-- #############################
-- # DISCRETE LINES AND SPACES #
-- #############################


-- | 1.1 LINES | --

-- A line with a focus.
-- Line xs y zs represents a descrete line:
--   xs represents all elements to the left (below)
--   y is the element in focus
--   zs represents all elements after (above)

data Line a = Line [a] a [a]
  deriving (Show) -- required to enable printing (for finite lines)

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]


-- ### EX. 1.1 ### --

-- | Keep up to a given number of elements in each direction in a line.
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]

cutLine :: Int -> Line a -> Line a
cutLine num (Line xs y zs) = Line (take num xs) y (take num zs)

-- ### EX. 1.2 ### --

-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce
-- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to
-- produce a list of elements to the right of x.

doWhileNotNothing :: (a -> Maybe a) -> a -> [a]
doWhileNotNothing func x =
  case (func x) of
    Nothing -> []
    Just a -> a:(doWhileNotNothing func a)

genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine generateXS y generateZS = Line (doWhileNotNothing generateXS y) y (doWhileNotNothing generateZS y)

-- ### EX. 1.3 ### --

-- | Apply a function to all elements on a line.
-- mapLine (^2) integers = Line [1, 4, 9, ..] 0 [1, 4, 9, ..]
mapLine :: (a -> b) -> Line a -> Line b
mapLine func (Line xs y zs) = Line (map func xs) (func y) (map func zs) 

-- ### EX. 1.4 ### --

-- | Zip together two lines.
-- zipLines integers integers
--   = Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..]

zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line xsA yA zsA) (Line xsB yB zsB) = Line (zip xsA xsB) (yA, yB) (zip zsA zsB)

-- | Zip together two lines with a given combining function.
-- zipLinesWith (*) integers integers
--   = Line [1,4,9,..] 0 [1,4,9,..]

zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith func (Line xsA yA zsA) (Line xsB yB zsB) = Line (zipWith func xsA xsB) (func yA yB) (zipWith func zsA zsB)

-- | 1.2 RULE 30 | --

data Cell = Alive | Dead
  deriving (Show)

renderCell :: Cell -> Picture
renderCell Alive = solidRectangle 1 1 
renderCell Dead = rectangle 1 1

-- ### EX. 1.5 ### --

rule30 :: Line Cell -> Cell
rule30 (Line (Dead:_) Dead (Dead:_)) = Dead
rule30 (Line (Dead:_) Dead (Alive:_)) = Alive
rule30 (Line (Dead:_) Alive (Dead:_)) = Alive
rule30 (Line (Dead:_) Alive (Alive:_)) = Alive
rule30 (Line (Alive:_) Dead (Dead:_)) = Alive
rule30 (Line (Alive:_) Dead (Alive:_)) = Dead
rule30 (Line (Alive:_) Alive (Dead:_)) = Dead
rule30 (Line (Alive:_) Alive (Alive:_)) = Dead
rule30 (Line xs y zs) = y

-- ### EX. 1.6 ### --

shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing
shiftLeft (Line (x:xs) y zs) = Just (Line xs x (y:zs))

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing
shiftRight (Line xs y (z:zs)) = Just (Line (y:xs) z zs)

-- ### EX. 1.7 ### --

lineShifts :: Line a -> Line (Line a)
lineShifts l = genLine shiftLeft l shiftRight 

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- ### EX. 1.8 ### --

-- | Render a line of 1x1 pictures.
renderLine :: Line Picture -> Picture
renderLine (Line xs y zs) = (translated (-1) 0 (renderPart (-1) xs)) <> y <> (translated 1 0 (renderPart 1 zs))
  where
    renderPart :: Double -> [Picture] -> Picture
    renderPart _ [] = blank
    renderPart shift (p:pics) = p <> (translated shift 0 (renderPart shift pics))
    
sampleLine :: Line Picture
sampleLine = Line [a,b,c,d,e,f,g] c [g,d,b,c,a,f]
  where
    a = colored red (solidCircle 0.5)
    b = colored green (solidCircle 0.5)
    c = colored blue (solidCircle 0.5)
    d = colored yellow (solidCircle 0.5)
    e = colored purple (solidCircle 0.5)
    f = colored brown (solidCircle 0.5)
    g = colored pink (solidCircle 0.5)

-- | Render the fist N steps of Rule 30,
-- applied to a given starting line.

renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 _ = blank
renderRule30 n l = (renderLine (mapLine renderCell l)) <> (translated 0 (-1) (renderRule30 (n-1) (applyRule30 l)))

sampleCellLine = Line (replicate 30 Dead) Alive (replicate 30 Dead)

-- | 1.3 DISCRETE SPACES | --

-- | A descrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line.
data Space a = Space (Line (Line a))

-- ### EX. 1.9 ### --

-- productOfLines :: Line a -> Line b -> Space (a, b)
-- productOfLines (Line xsA yA zsA) (Line xsB yB zsB) = 

-- ### EX. 1.10 & 1.11 ### --

mapSpace :: (a -> b) -> Space a -> Space b
mapSpace func (Space (Line xs y zs)) = (Space (Line 
  (map (\x -> mapLine func x) xs) 
  (mapLine func y) 
  (map (\z -> mapLine func z) zs)))

zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line xsA yA zsA)) (Space (Line xsB yB zsB)) = (Space (Line 
  (zipWith zipLines xsA xsB) 
  (zipLines yA yB) 
  (zipWith zipLines zsA zsB)))

zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith func (Space (Line xsA yA zsA)) (Space (Line xsB yB zsB)) = (Space (Line
  (zipWith (zipLinesWith func) xsA xsB)
  (zipLinesWith func yA yB)
  (zipWith (zipLinesWith func) zsA zsB))) 

-- | 1.4 CONWAY’S GAME OF LIFE | --

-- ### EX. 1.12 ### --

toInt :: Cell -> Int
toInt Alive = 1
toInt Dead = 0

countAlive :: Line (Line Cell) -> Int
countAlive (Line xsLine y zsLine) = (countAliveFromPart xsLine) + (countAliveFromLine y) + (countAliveFromPart zsLine)
  where 
    countAliveFromPart :: [Line Cell] -> Int
    countAliveFromPart [] = 0
    countAliveFromPart ((Line [] y []):ls) = toInt y
    countAliveFromPart ((Line (x:xs) y []):ls) = (toInt y) + (toInt x)
    countAliveFromPart ((Line [] y (z:zs)):ls) = (toInt y) + (toInt z)
    countAliveFromPart ((Line (x:xs) y (z:zs)):ls) = (toInt x) + (toInt y) + (toInt z)
    
    countAliveFromLine :: Line Cell -> Int
    countAliveFromLine (Line [] y []) = 0
    countAliveFromLine (Line [] y (z:zs)) = toInt z
    countAliveFromLine (Line (x:xs) y []) = toInt x
    countAliveFromLine (Line (x:xs) y (z:zs)) = (toInt x) + (toInt z)

conwayRule :: Space Cell -> Cell
conwayRule (Space (Line xsLines (Line xs Alive zs) zsLines))
  | numOfAlive == 2 = Alive
  | numOfAlive == 3 = Alive
  | otherwise = Dead
    where
      numOfAlive = countAlive (Line xsLines (Line xs Alive zs) zsLines)

conwayRule (Space (Line xsLines (Line xs Dead zs) zsLines))
  | numOfAlive >= 3 = Alive
  | otherwise = Dead
    where
      numOfAlive = countAlive (Line xsLines (Line xs Dead zs) zsLines)

-- ### EX. 1.13 ### --

spaceShiftUp :: Space a -> Maybe (Space a)
spaceShiftUp (Space (Line xsLines y zsLines)) = 
  case (shiftLeft y) of
    Nothing -> Nothing
    Just yNew -> Just (Space (Line (map shift xsLines) yNew (map shift zsLines)))
      where 
        shift :: Line a -> Line a
        shift (Line (x:xs) y zs) = (Line xs x (y:zs))
  
spaceShiftDown :: Space a -> Maybe (Space a)
spaceShiftDown (Space (Line xsLines y zsLines)) = 
  case (shiftRight y) of
    Nothing -> Nothing
    Just yNew -> Just (Space (Line (map shift xsLines) yNew (map shift zsLines)))
      where 
        shift :: Line a -> Line a
        shift (Line xs y (z:zs)) = (Line (y:xs) z zs)


spaceShifts :: Space a -> Space (Space a)
spaceShifts (Space (Line xsLines y zsLines))= (Space (Line
  (genLine shiftLeft (Space (Line xsLines )
  (genLine spaceShiftUp (Space (Line xsLines y zsLines)) spaceShiftDown)
  ()))
 
 
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- ### EX. 1.14 ### --

-- | Render a space of 1x1 pictures.

renderSpace :: Space Picture -> Picture
renderSpace (Space (Line xs y zs)) = (renderPart 1 (map renderLine xs) ) <> (renderLine y) <> (renderPart (-1) (map renderLine zs) )
  where 
    renderPart :: Double -> [Picture] -> Picture
    renderPart _ [] = blank
    renderPart shift (p:pics) = p <> (translated 0 shift (renderPart shift pics))

-- | Animate Conway's Game of Life,
-- starting with a given space
-- and updating it every second.

--animateConway :: Space Cell -> IO ()


main = print(1)

