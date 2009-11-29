module LineTools where

data Square = Black | White | Any deriving Eq
data LineInfo = LineInfo { lineLen :: Int, hints :: [Int] } deriving Show

type Line = [Square]
type Grid = [Line]

squareToChar Black   = 'X'
squareToChar White   = '.'
squareToChar Any     = ' '

-- Shows square lists as strings
instance Show Square where
  show a = [squareToChar a]
  showList [] str = str
  showList list str = '|':foldr consSq ('|':str) list
    where consSq x = (squareToChar x:) 

-- "Ands" two squares
sqAnd :: Square -> Square -> Square
sqAnd White White = White
sqAnd Black Black = Black
sqAnd _ _ = Any

-- Checks if two squares are "compatible". Any may transform to either Black or
-- White so it is compatible with both of them.
sqOk :: Square -> Square -> Bool
sqOk White Black = False
sqOk Black White = False
sqOk _ _ = True

-- "Ands" a set of lines.
lineAnd :: [Line] -> Line
lineAnd = foldl1 (zipWith sqAnd)

-- Returns number of Any squares from a line
unknown :: Line -> Int
unknown line = length $ filter (==Any) line

-- Checks if the lines are not "contraversial"
-- Used to filter out impossible combinations with previous state
-- while iterating.
lineOk :: Line -> Line -> Bool
lineOk a b = and $ zipWith sqOk a b

-- Produces Grid to a string ready to be printed to screen
showGrid :: Grid -> String
showGrid grid = unlines $ ([topline] ++ map show grid ++ [topline])
  where topline = replicate (2+(length $ head grid)) '-'