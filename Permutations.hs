module Uusiyritys where

import Data.List

perm [] = [[]]
perm lista = [(x:xs) | x <- lista, xs <- perm (delete x lista)]

-- Returns list of possible block positions [(position,length)]
iters lineLen _ [] = [[(lineLen,0)]]
iters lineLen curPos (curLen:endLens) =
  [((x-curPos,curLen):xs) |
   x <- [curPos..lineLen-endBlockLen],
   xs <- iters lineLen (1+x+curLen) endLens]
    where endBlockLen = curLen + (sum endLens) + (length endLens)

--blackWhite ((pos, len):xs