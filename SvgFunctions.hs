module SvgFunctions where
import Hadijatek

import Data.List
import Data.Tuple
import Data.Matrix as M
import Data.Maybe
import Data.Either


sortFieldsByType :: Fields -> Fields
sortFieldsByType fields = sortBy (\field1 field2 -> compare (fieldType field1) (fieldType field2)) fields

distanceToPolygon :: Coordinate -> [Coordinate] -> Float
distanceToPolygon point coords = -- 
   (head . sort) [(abs ((x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1))) / distance (coord i) (coord (i+1)) --(sqrt ((x2 - x1)^2 + (y2 - y1)^2))
                  | i <- [0..(length coords -1)],
                    let coord n = coords !! (n `mod` length coords),
                    let (x0, y0) = point,
                    let (x1, y1) = coord i,
                    let (x2, y2) = coord (i+1)]-- 

intersectsWithPolygon :: (Coordinate, Coordinate) -> [Coordinate] -> Int
intersectsWithPolygon (p3, p4) coords = length-- 
  [coord i
   | i <- [0..(length coords -1)],
     let coord n = coords !! (n `mod` length coords),
     let x1 = fst (coord i), let y1 = snd (coord i),
     let x2 = fst (coord (i+1)), let y2 = snd (coord (i+1)),
     let (x3, y3) = p3, let (x4, y4) = p4,
     -- foldl (||) False [ps !! j /= ps !! k | let ps = [(x1, y1), (x2, y2), p3, p4], j <- [0..(length ps -1)], k <- [0..(length ps -1)], j /= k],
     not (p3 `elem` [(x1,y2),(x2,y2)] || p4 `elem` [(x1,y2),(x2,y2)]), -- Avoids repeat of intersection at vertex
     let t = (detLU $ fromLists [[x1-x3,x3-x4],[y1-y3,y3-y4]]) / (detLU $ fromLists [[x1-x2,x3-x4],[y1-y2,y3-y4]]),
     let u = (detLU $ fromLists [[x1-x3,x1-x2],[y1-y3,y1-y2]]) / (detLU $ fromLists [[x1-x2,x3-x4],[y1-y2,y3-y4]]),
     u >= 0 && u <= 1, t >= 0 && t <= 1]-- 

area :: [Coordinate] -> Float
area coords = 1/2 * sum-- 
  [(fst (coord i) * snd (coord (i+1))) - (fst (coord (i+1)) * snd (coord i))
   | i <- [0..(length coords -1)],
     let coord n = coords !! (n `mod` length coords)]-- 

isClockwise :: [Coordinate] -> Bool
isClockwise coords = area coords <= 0

within :: Coordinate -> Coordinate -> Bool
within (x0, y0) (x1, y1) = x0 <= x1 && y0 <= y1

cullToConvex :: [Coordinate] -> [Coordinate]
cullToConvex coords -- = culledCoords
  | isConvex = coords
  | length culledPolys == 0 = coords
  | otherwise = cullToConvex culledCoords
    where
      sortByArea = sortBy (\poly1 poly2 -> compare (abs $ area poly1) (abs $ area poly2))
      coord n = coords !! (n `mod` length coords)
      lcoords = length coords
      convexAngles poly = length . filter snd $
        [(vecang (p (i-1)) (p i) (p (i+1)),
          not $ isClockwise coords `xor` isClockwise [p (i-1),p i,p (i+1)])
         | i <- [0..(length poly -1)], 
           let p n = poly !! (n `mod` length poly)]
      concaveAngles poly = length poly - convexAngles poly
      isConvex = lcoords == convexAngles coords
      isNotMoreConcave poly1 poly2 = concaveAngles poly1 <= concaveAngles poly2
      isMoreConvex poly1 poly2 = concaveAngles poly1 < concaveAngles poly2
      culledPolys' =
        [poly
         | j <- [0..(lcoords-1)],
           not $ isClockwise coords `xor` isClockwise [coord (j-1),coord j,coord (j+1)],
           let poly = [coord i
                       | i <- [0..(lcoords-1)],
                         i /= j --,
                         ],
                         -- not $ isClockwise coords `xor` isClockwise [coord (i-1),coord i,coord (i+1)]],
           poly `isNotMoreConcave` coords,
           not $ isClockwise coords `xor` isClockwise poly]
      moreConvexCulledPolys = filter (\poly -> poly `isMoreConvex` coords) culledPolys'
      culledPolys = if length moreConvexCulledPolys /= 0 then moreConvexCulledPolys else culledPolys'
      culledCoords = head . reverse . sortByArea $ culledPolys-- 

centroid :: [Coordinate] -> Coordinate
centroid coords = (c fst, c snd)-- 
  where
    a = area coords
    c nth = 1/(6*a) * sum [(nth (coord i) + nth (coord (i+1))) * (fst (coord i) * snd (coord (i+1)) - fst (coord (i+1)) * snd (coord i))
                           | i <- [0..(length coords -1)],
                             let coord n = coords !! (n `mod` length coords)]-- 

centerOfPolygon :: [Coordinate] -> Coordinate -- NOT GOOD picks best "center" of polygon, by testing which is furthest in(land/sea)
centerOfPolygon coords = -- 
  head .
  sortBy (\c1 c2 -> compare (distanceToPolygon c2 coords) (distanceToPolygon c1 coords)) .
  filter (`inPolygon` coords) . 
  map centroid $
  [coords, removeStrait coords, cullToConvex . removeStrait $ coords]-- 

removeStrait :: [Coordinate] -> [Coordinate] -- returns side of strait with larger area
removeStrait coords-- 
  | isStrait coords = newCoords
  | otherwise = coords
    where
      straitPoints = filter (\p -> 2 == count p coords) coords -- there should only be 4 of these
      poly1 = takeUntil (straitPoints !! 0) coords ++ dropUntilN 2 (straitPoints !! 1) coords
      poly2 = takeUntil (straitPoints !! 1) $ dropUntil (straitPoints !! 0) coords
      newCoords = if abs (area poly1) >= abs (area poly2) then poly1 else poly2-- 

createStraitLines :: [Coordinate] -> ((Coordinate, Coordinate), (Coordinate, Coordinate)) -- TEST FOR STRAIT-NESS FIRST
createStraitLines coords = ((pn (+) pl1, pn (-) pl1), (pn (+) pl2, pn (-) pl2))-- 
  where
    straitPoints = take 2 $ filter (\p -> 2 == count p coords) coords
    pl1 = straitPoints !! 0
    pl2 = straitPoints !! 1
    m = (fst pl1 - fst pl2) / (snd pl2 - snd pl1)
    llines = 
     [(pl1, last . takeUntil pl1 $ coords),
      (pl2, head . dropUntil pl2 $ coords),
      (pl2, last . takeUntil pl2 . dropUntil pl2 $ coords),
      (pl1, head . dropUntilN 2 pl1 $ coords)]
    l = last . take 3 . sort . map (uncurry distance) $ llines
    dx = l / sqrt (1 + m^2)
    dy = m * dx
    pn o pl = (o (fst pl) dx, o (snd pl) dy)-- 

