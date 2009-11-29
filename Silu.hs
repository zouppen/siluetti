module Silu where

import Data.List
import Permutations
import LineTools

--solve :: ([[Int]],[[Int]]) -> Grid
solve (rawX,rawY) = getSolution $ iterate (iterateBoth linesX linesY) initgrid 
  where linesX = toLineInfo rawX rawY
        linesY = toLineInfo rawY rawX
        initgrid = replicate (length rawX) $ replicate (length rawY) Any  

-- Takes steps while there are some progress (next steps is different from
-- previous)
getGoodSteps (x1:x2:xs) | (x1 == x2) = [x1]
                        | otherwise = x1:(getGoodSteps (x2:xs))

-- Takes the last step. This is either the final solution or the most complete
-- result this horizontal-vertical iteration can do.
getSolution list = last $ getGoodSteps list

-- Combined iteration step horizontal + vertical
iterateBoth :: [LineInfo] -> [LineInfo] -> Grid -> Grid
iterateBoth infoX infoY grid = iterateV infoY $ iterateH infoX grid

-- Horizontal iteration step.
iterateH :: [LineInfo] -> Grid -> Grid
iterateH infos grid = zipWith matchIterate infos grid

-- Vertical iteration step. Transposes the grid and uses horizontal iteration.
iterateV :: [LineInfo] -> Grid -> Grid
iterateV infos grid = transpose $ iterateH infos $ transpose grid

-- Makes a Lineinfo out of simple horizontal-vertical bar counts
toLineInfo :: [[Int]] -> [[Int]] -> [LineInfo]
toLineInfo this other = map mapper this 
  where mapper x = LineInfo (length other) x
        
-- It's the magic behind everything. Produces only the results that are
-- compatible with previous step and "ands" the list to get a new step
matchIterate :: LineInfo -> Line -> Line
matchIterate info previous = lineAnd $ filter (lineOk previous) $
                             map blackWhiter $ iters info