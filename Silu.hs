module Silu where

import Data.List
import Permutations
import LineTools

--solve :: ([[Int]],[[Int]]) -> Grid
solve (rawX,rawY) = (iterateBoth linesX linesY) initgrid 
  where linesX = toLineInfo rawX rawY
        linesY = toLineInfo rawY rawX
        initgrid = replicate (length rawX) $ replicate (length rawY) Any  

iterateBoth :: [LineInfo] -> [LineInfo] -> Grid -> Grid
iterateBoth infoX infoY grid = iterateV infoY $ iterateH infoX grid
        
iterateH :: [LineInfo] -> Grid -> Grid
iterateH infos grid = zipWith matchIterate infos grid

iterateV :: [LineInfo] -> Grid -> Grid
iterateV infos grid = transpose $ iterateH infos $ transpose grid

toLineInfo :: [[Int]] -> [[Int]] -> [LineInfo]
toLineInfo this other = map mapper this 
  where mapper x = LineInfo (length other) x
        
matchIterate :: LineInfo -> Line -> Line
matchIterate info previous = lineAnd $ filter (lineOk previous) $
                             map blackWhiter $ iters info