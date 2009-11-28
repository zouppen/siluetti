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

--iterations :: LineInfo -> [Line]
--iterations info = 
  
--iterations' (LineInfo len (hint:hints)) left 

freedom (LineInfo len hints) = len - mass
  where mass = (length hints - 1) + sum hints

movement :: Int -> Int -> [[Int]]
movement _ 0 = []
movement free bars = [(n:movement (free-n) (bars-1)) | n <- [0..free]]

clock :: (b -> Maybe (a, b))
clock (0,(x1:xs)) = 
clock (0,(x1:x2:xs)) = Just (x1:x2:xs, (x1,(0:x2+1:xs)))
clock (n,(x1:xs)) = Just (x1:xs,(n-1,x1+n:xs))

limits :: Int -> Int -> Int -> Int -> [Line]
limits leftLimit rightLimit runLen lineLen =
  [capsule ((replicate runLen Black) ++ (replicate movability Any)),
  capsule ((replicate movability Any) ++ (replicate runLen Black))]
  where capsule line = replicate leftLimit Any ++
                       line ++
                       replicate rightLimit Any
        movability = lineLen - leftLimit - rightLimit - runLen

2, 0 0 0
1, 1 0 0
0, 2 0 0
1, 0 1 0
0, 1 1 0
0, 0 1 1 
   0 0 2


2, 0 ..
     
1, 1
0, 2
XX-

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