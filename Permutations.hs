module Permutations where

import Data.List
import LineTools

perm [] = [[]]
perm lista = [(x:xs) | x <- lista, xs <- perm (delete x lista)]

-- Returns list of possible block positions [(position,length)]
iters lineLen barLens = iters' lineLen 0 barLens
iters' lineLen curPos [] = [[(0,lineLen-curPos+1)]]
iters' lineLen curPos (curLen:endLens) =
  [((x,curLen):xs) |
   x <- [curPos..lineLen-endBlockLen],
   xs <- iters' lineLen (1+x+curLen) endLens]
    where endBlockLen = curLen + (sum endLens) + (length endLens)

blackWhiter :: [(Int,Int)] -> [Square]
blackWhiter [] = []
blackWhiter ((0,0):xs) = blackWhiter xs
blackWhiter ((0,blacks):xs) = Black:blackWhiter ((0,blacks-1):xs)
blackWhiter ((whites,blacks):xs) = White:blackWhiter ((whites-1,blacks):xs)
