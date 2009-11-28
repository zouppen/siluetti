module LineTools where

data Square = Black | White | Any

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

sqComb White Black = error "Impossible"
sqComb Black White = error "Impossible"
sqComb Black Black = Black
sqComb White White = White
sqComb Any a = a
sqComb a Any = a

lineAnd :: [Line] -> Line
lineAnd = foldl1 (zipWith sqAnd)