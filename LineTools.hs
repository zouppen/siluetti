module LineTools where

data Square = Black | White | Any deriving Eq

data LineInfo = LineInfo { lineLen :: Int, hints :: [Int] } deriving Show
--data LinePosInfo = LinePosInfo { lineLen :: Int,
--                                 poshints :: [(Int,Int)] } deriving Show

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

sqAnd :: Square -> Square -> Square
sqAnd White White = White
sqAnd Black Black = Black
sqAnd _ _ = Any

sqOk White Black = False
sqOk Black White = False
sqOk Black Black = True
sqOk White White = True
sqOk Any _ = True
sqOk _ Any = True

lineAnd :: [Line] -> Line
lineAnd = foldl1 (zipWith sqAnd)

unknown :: Line -> Int
unknown line = length $ filter (==Any) line

-- Checks if the lines are not "contraversial"
lineOk :: Line -> Line -> Bool
lineOk a b = and $ zipWith sqOk a b
