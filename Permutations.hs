module Permutations where

import Data.List
import LineTools

-- Returns list of possible block positions [(position,length)]
iters :: LineInfo -> [[(Int, Int)]]
iters (LineInfo lineLen barLens) = iters' lineLen (-1) barLens

iters' lineLen curPos [] = [[(lineLen-curPos,0)]]
iters' lineLen curPos (curLen:endLens) =
  [((x-curPos,curLen):xs) |
   x <- [curPos+1..lineLen-endBlockLen],
   xs <- iters' lineLen (x+curLen) endLens]
    where endBlockLen = curLen + (sum endLens) + (length endLens)

-- Generates black-white list from list of (blacks,whites) pairs
-- removes the extra head produced by 'iters'.
blackWhiter :: [(Int,Int)] -> [Square]
blackWhiter = tail.blackWhiter'

blackWhiter' [] = []
blackWhiter' ((0,0):xs) = blackWhiter' xs
blackWhiter' ((0,blacks):xs) = Black:blackWhiter' ((0,blacks-1):xs)
blackWhiter' ((whites,blacks):xs) = White:blackWhiter' ((whites-1,blacks):xs)