{-# LANGUAGE ParallelListComp #-}

module Hadijatek where
import Data.List
import Data.Tuple
import Data.Matrix as M
import Data.Maybe
import Data.Either

data Team = Team
      {teamID :: Int,
     teamName :: String,
    teamColor :: String,
      baseIDs :: [(Int, Bool)]} -- ID, isHomeBase
  deriving Eq

data Field = Field
     {fieldID :: Int,
    fieldType :: Int,
    fieldName :: String,
   fieldColor :: String,
    fieldRoot :: Coordinate,
         path :: String}
  deriving Eq

data Unit = Unit
   {unitField :: Int,
     unitTeam :: Int,
     unitType :: Int}
  deriving Eq

data OrderStatus = Unresolved | Failed deriving (Eq, Show)
data Order = Order
  {orderField :: Int,
    orderType :: Int, -- if it's -1 (retreat/death) or 5 (new unit), bool should be true if the number of units is to change (e.g. death)
 orderAffects :: (Bool, [Int]), -- (Conquers, Dies, letsAllPass (failed static order), newUnitType : fieldIDs and/or teamIDs - LAST ELEMENT MUST BE fieldID OF WHERE UNIT ENDS ITS TURN)
  orderStatus :: OrderStatus}
  deriving Eq

type Coordinate = (Float, Float)

type Lang = Int

type Teams = [Team]
type Units = [Unit]
type Fields = [Field]
type Turn = (Int, [Order]) -- TeamID, their orders - pretty useless
type Orders = [Order]
type Adjacencies = Matrix Int

instance Show Team where
  show (Team i n c bs) = -- 
    "{teamID=" ++ show i ++
    ", teamName=\"" ++ n ++
    "\", teamColor=\"" ++ c ++
    "\", baseIDs=" ++ show bs ++ "}"-- 

instance Show Unit where
  show (Unit f team t) = -- 
    "{unitField=" ++ show f ++
    ", unitTeam=" ++ show team ++
    ", unitType=" ++ show t ++ "}"-- 

instance Show Field where
  show (Field i t n c r p) = -- 
    "{fieldID=" ++ show i ++
    ", fieldType=" ++ show t ++
    ", fieldName=\"" ++ n ++
    "\", fieldColor=\"" ++ c ++
    "\", fieldRoot=" ++ show r ++
    ", path=\"" ++ p ++ "\"}"-- 

instance Show Order where
  show (Order i t a s) = -- 
    "{orderField=" ++ show i ++
    ", orderType=" ++ show t ++
    ", orderAffects=" ++ show a ++
    ", orderStatus=" ++ show s ++ "}"-- 

-- Entry creation for .hmap file
teamToEntry :: Team -> String
teamToEntry (Team i n c bs) = show i ++ ",\"" ++ n ++ "\",\"" ++ c ++ "\"," ++ show bs

unitToEntry :: Unit -> String
unitToEntry (Unit f team t) = show f ++ "," ++ show team ++ "," ++ show t

fieldToEntry :: Field -> String
fieldToEntry (Field i t n c r p) = show i ++ "," ++ show t ++ ",\"" ++ n ++ "\"," ++ show c ++ "," ++ show r ++ "," ++ show p

orderToEntry :: Order -> String
orderToEntry (Order i t a s) = show i ++ "," ++ show t ++ "," ++ show a

matrixToEntries :: Adjacencies -> [String]
matrixToEntries matrix = map show $ M.toLists matrix

-- Entry retrieval form .hmap or .turn file
readTeam :: String -> Team
readTeam entry = -- 
  Team (read (takeUntil ',' entry) :: Int)
       (takeUntil '"' (dropUntil '"' entry))
       (takeUntil '"' (dropUntil '"' (dropUntilN 2 ',' entry)))
       (read (dropUntilN 3 ',' entry) :: [(Int, Bool)])-- 

readUnit :: String -> Unit
readUnit entry = -- 
  Unit (read (takeUntil ',' entry) :: Int)
       (read (takeUntil ',' $ dropUntil ',' entry) :: Int)
       (read (takeUntil ',' $ dropUntilN 2 ',' entry) :: Int)-- 

readField :: String -> Field
readField entry = -- 
  Field (read (takeUntil ',' entry) :: Int)
        (read (takeUntil ',' $ dropUntil ',' entry) :: Int)
        (takeUntil '"' $ dropUntil '"' entry)
        (takeUntil '"' . tail . dropUntilN 3 ',' $ entry)
        (read (init . init . takeUntils ",\"" $ dropUntilN 4 ',' entry) :: (Float, Float))
        (init . tail . dropUntilN 6 ',' $ entry)-- 

readOrder :: String -> Order
readOrder entry = -- 
  Order (read (takeUntil ',' entry) :: Int)
        (read (takeUntil ',' $ dropUntil ',' entry) :: Int)
        (read (dropUntilN 2 ',' entry) :: (Bool, [Int]))
        Unresolved-- 

-- readMatrix :: String -> Adjacencies
-- readMatrix entries = fromLists $ map (\line -> read line :: [Int]) $ lines entries

-- Fetch all instances of data from .hmap or .turn
fetchTeams :: String -> Teams -- input whole .hmap
fetchTeams hmap = map readTeam $ takeUntil "---" $ dropUntil "---" $ lines hmap

fetchUnits :: String -> Units -- input whole .hmap
fetchUnits hmap = map readUnit $ takeUntil "---" $ dropUntilN 2 "---" $ lines hmap

fetchFields :: String -> Fields -- input whole .hmap
fetchFields hmap = map readField $ dropUntilN 3 "---" $ lines hmap

fetchOrders :: String -> Orders -- teamID (from file name), input whole .turn
fetchOrders turn = map readOrder . lines $ turn

fetchTurn :: Int -> String -> Turn -- teamID (from file name), input whole .turn
fetchTurn tid turn = (tid, map readOrder . lines $ turn)

-- all data to hmap file
dataToHmap :: Lang -> Int -> String -> String -> String -> Teams -> Fields -> Units -> String
dataToHmap lang step name waterStrokeColor landStrokeColor teams fields units = -- 
  ["Név: ", "Name: "] !! lang ++ name ++
  '\n' : (["Lépések: ", "Steps: "] !! lang) ++ show step ++ 
  '\n' : (["Tengeri mezők körvonalának Színe: ", "Water Stroke Color: "] !! lang) ++ waterStrokeColor ++
  '\n' : (["Szárazföld körvonal Szín: ", "Land Stroke Color: "] !! lang) ++ landStrokeColor ++
  "\n---\n" ++
  unlines (map teamToEntry teams) ++ "---\n" ++
  unlines (map unitToEntry units) ++ "---\n" ++
  unlines (map fieldToEntry fields) --

-- Matrix to the Nth power
matrixNth :: Num a => Matrix a -> Int -> Matrix a
matrixNth m 1 = m
matrixNth m n = multStd2 m $ matrixNth m (n-1)

-- Which team occupiest the field?
fieldOccupiedBy :: Teams -> Either Field Int -> Maybe Int
fieldOccupiedBy teams field = elemIndex [fid] $ map (filter (==fid) . map fst . baseIDs) teams-- 
  where fid = either fieldID id field-- 

-- Title of field for mouse hovering in SVG
fieldTitle :: Lang -> Teams -> Field -> String
fieldTitle lang teams field = -- 
  fieldName field ++
  if occupier == Nothing
    then if fieldType field == 2 then "&#10;" ++ ["Foglalatlan", "Unoccupied"] !! lang else ""
    else "&#10;" ++ teamName (teams !! (fromJust occupier))
  where occupier = fieldOccupiedBy teams (Left field)-- 

teamOfOrder :: Teams -> Units -> Order -> Maybe Team -- Returns team that issued the order, Nothing if invalid
teamOfOrder teams units order = -- 
  if tidUnit == []
    then
      if tidHomeBase == []
        then Nothing
        else Just $ teams !! head tidHomeBase
    else Just $ teams !! head tidUnit
  where
    tidUnit = map unitTeam . filter (\u -> orderField order == unitField u) $ units -- Team ID in a list, if unit exists
    tidHomeBase = map teamID . filter (\t -> (orderField order, True) `elem` baseIDs t) $ teams -- Team ID in a list, if ordered on home base

-- Name of unit
unitName :: Lang -> Either Unit Int -> String
unitName lang unit = -- 
 case t of
  0 -> "Tank"
  1 -> ["Hajó", "Ship"] !! lang
  2 -> ["Repülő", "Plane"] !! lang
  3 -> ["Szupertank", "Supertank"] !! lang
  4 -> ["Tengeralattjáró", "Submarine"] !! lang
  5 -> ["Tüzérség", "Artillery"] !! lang
 where t = either unitType id unit-- 

-- Name of order - retreat & death not included
orderName :: Lang -> Either Order Int -> String
orderName lang order = -- 
 case t of
  0 -> ["Támad", "Attacks"] !! lang
  1 -> ["Véd", "Defends"] !! lang
  2 -> ["Támogat", "Supports"] !! lang
  3 -> ["Átalakul", "Transforms"] !! lang
  4 -> ["Zárótűz", "Bombards"] !! lang
  5 -> ["Felállít", "Summoned"] !! lang
 where t = either orderType id order-- 

showOrderToActions :: Lang -> Teams -> Fields -> Units -> Order -> String 
showOrderToActions lang teams fields units (Order f _ (_,_) _) = -- 
  fieldName (fields !! f) ++ " " ++
  teamName (fromJust . teamOfOrder teams units $ order) ++ " " ++
  unitName lang (Left . head . filter (\u -> unitField u == f) $ units) ++ " "
  where
    order = Order f 0 (True,[]) Unresolved-- 
  
showOrderToTargets :: Lang -> Teams -> Fields -> Units -> Order -> String
showOrderToTargets lang teams fields units (Order f t (_,_) _) = -- 
  showOrderToActions lang teams fields units (Order f 0 (False,[]) Unresolved) ++
  orderName lang (Right t) ++ " "
  where
    order = Order f t (True,[]) Unresolved-- 

showOrderToAffects :: Lang -> Teams -> Fields -> Units -> Order -> String
showOrderToAffects lang teams fields units (Order f t (b,as) _) = -- 
  showOrderToTargets lang teams fields units (Order f t (False,[]) Unresolved) ++
  (if t /= 3
   then if t /= 1 then fieldName (fields !! last as) else "" -- If anything but transforming, say fields
   else unitName lang (Right $ last as)) -- Otherwise, the unit name
  ++ 
  foldr (uncurry if') "" [
    (t == 0,if length as == 2
             then [" ", " through "] !! lang else ""),
    (t == 1,[", átengedve ", ", letting "] !! lang),
    (t == 2," (" )]
  where
    order = Order f t (b,as) Unresolved-- 

showOrderPretty :: Lang -> Teams -> Fields -> Units -> Order -> String
showOrderPretty lang teams fields units (Order f t (b,as) _) = -- 
  showOrderToAffects lang teams fields units order ++
  foldr (uncurry if') "" [
    (t == 0,if length as == 2
              then 
                (fieldName $ fields !! head as) ++ -- fields unit flies over -- currently max. 1
                ["-n át",""] !! lang
              else "" ++
            if b then "" else ["(Nem foglal)","(Does not conquer)"] !! lang),
    (t == 1,(drop 2 . concat . map (\tid -> ", " ++ teamName (teams !! tid)) . init $ as) ++
              if b then ["mindenkit"," everyone"] !! lang else "" ++
             ["", " through"] !! lang ), -- List all teams which are let through
    (t == 2, teamName (teams !! head as) ++ ")")] -- The team supported
  where
    order = Order f t (b,as) Unresolved-- 

genNextOrderFiles :: String -> Int -> Teams -> IO()
genNextOrderFiles name step teams = do-- 
  let names = [name ++ "_" ++ show step ++ "_" ++ show (teamID team) ++ ".turn" | team <- teams]
  mapM (\n -> writeFile n "") names
  return () --

inPolygon :: Coordinate -> [Coordinate] -> Bool
inPolygon (x3, y3) coords = intersects `mod` 2 == 1-- 
  where 
    intersects = length [coord i
                         | i <- [0..(length coords -1)],
                           let coord n = coords !! (n `mod` length coords),
                           snd (coord i) /= y3, -- Avoids repeat of intersection at vertex
                           let x1 = fst (coord i), let y1 = snd (coord i),
                           let x2 = fst (coord (i+1)), let y2 = snd (coord (i+1)),
                           let x4 = minimum [x3, x1, x2], let y4 = y3,
                           let t = (detLU $ fromLists [[x1-x3,x3-x4],[y1-y3,y3-y4]]) / (detLU $ fromLists [[x1-x2,x3-x4],[y1-y2,y3-y4]]),
                           let u = (detLU $ fromLists [[x1-x3,x1-x2],[y1-y3,y1-y2]]) / (detLU $ fromLists [[x1-x2,x3-x4],[y1-y2,y3-y4]]),
                           u >= 0 && u <= 1, t >= 0 && t <= 1]-- 

fieldsAreNeighbors :: Field -> Field -> Bool
fieldsAreNeighbors (Field _ t1 _ _ _ path1) (Field _ t2 _ _ _ path2) = -- 
  (1 < length (intersect coord1 coord2) -- If there are at least 2 overlapping coordinates
   || (any
        (\((tl, cl), (tw, cw)) -> -- Or one contains the other (land is an island)
          (tl `elem` [1,2] && tw == 0) && all (`inPolygon` cw) cl)
        [((t1,coord1),(t2,coord2)), ((t2,coord2),(t1,coord1))] ) ) 
  && coord1 /= coord2 
  where
    coord1 = pathToCoordinates path1
    coord2 = pathToCoordinates path2-- 

getAdjacency :: Fields -> Adjacencies -- Returns regular adjacencies
getAdjacency fields = elementwise (+) adjs $ M.transpose adjs-- 
  where
    adjs = M.fromLists 
      [[if i > j
          then if fieldsAreNeighbors (fields !! i) (fields !! j) then 1 else 0
          else 0           -- Need to mirror
        | i <- [0..(length fields -1)] ]
       | j <- [0..(length fields -1)] ]-- 

getWaterAdjacency :: Fields -> Adjacencies
getWaterAdjacency fields = elementwise (+) wadjs $ M.transpose wadjs-- 
  where
    wadjs = M.fromLists
      [[if i > j
          then
            if fieldsAreNeighbors fi fj &&
                (any isWater [fi,fj] ||
                 sharePointWithWater)
              then 1 else 0
          else 0
        | i <- [0..(length fields -1)],
          let fi = fields !! i,
          let fj = fields !! j,
          let isWater = \f -> fieldType f == 0,
          let coords = pathToCoordinates . path,
          let sharePointWithWater = 
                any ( /= []) .
                map (intersect $ intersect (coords fi) (coords fj)) .
                map coords .
                filter isWater $
                fields,
          let adjWaters = \f -> filter isWater . filter (fieldsAreNeighbors f) $ fields ]
       | j <- [0..(length fields -1)] ]-- 

pathToCommands :: String -> [String]
pathToCommands [] = []-- 
pathToCommands path = command : pathToCommands restPath
  where
    command = take 2 path ++ (takeUntil ' ' (dropUntil ' ' path))
    restPath = drop (length command + 1) path-- 

commandsToCoordinates :: [String] -> [Coordinate]
commandsToCoordinates commands = -- 
  [foldr (uncurry if')
         (x i, y i)
         [(c i == 'H', (z i,lookback i)),
          (c i == 'V', (lookback i,z i))]
   | i <- [0..(length commands -1)],
     c i /= 'Z']
    where
      c n = head (commands !! n)
      coord n = dropUntil ' ' (commands !! n)
      x n = read (takeUntil ',' (coord n)) :: Float
      y n = read (dropUntil ',' (coord n)) :: Float
      z n = read (coord n) :: Float
      lookback n = -- 
        foldr (uncurry if')
          (0.0)
          [(c n == 'H' && (c (n-1) == 'L' || c (n-1) == 'M'), y (n-1)),
           (c n == 'H' && c (n-1) == 'H', lookback (n-1)),
           (c n == 'H', z (n-1)),
           (c n == 'V' && (c (n-1) == 'L' || c (n-1) == 'M'), x (n-1)),
           (c n == 'V' && c (n-1) == 'V', lookback (n-1)),
           (c n == 'V', z (n-1))]-- }}}

pathToCoordinates :: String -> [Coordinate]
pathToCoordinates path = (commandsToCoordinates . pathToCommands) path

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (c, a, b) -> a
snd' (_, a, _) = a

trd' :: (b, c, a) -> a
trd' (_, _, a) = a

xor :: Bool -> Bool -> Bool
xor a b = (a||b) && (not (a&&b))

distance :: Coordinate -> Coordinate -> Float
distance (x1,y1) (x2,y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

vecop' :: (Int -> Int -> Int) -> (Int,Int) -> (Int,Int) -> (Int,Int)
vecop' o (x1,y1) (x0,y0) = (o x1 x0,o y1 y0)

vecop :: (Float -> Float -> Float) -> Coordinate -> Coordinate -> Coordinate
vecop o (x1,y1) (x0,y0) = (o x1 x0,o y1 y0)

vecdot :: Coordinate -> Coordinate -> Float
vecdot (x1,y1) (x0,y0) = x0 * x1 + y0 * y1

vecang :: Coordinate -> Coordinate -> Coordinate -> Float -- cos of angle between three points
vecang a b c = vecdot (vecop (-) a b) (vecop (-) c b) / (distance a b * distance c b)

isStrait :: [Coordinate] -> Bool
isStrait coords = length coords - length (uniques coords) >= 2

isFieldStrait :: Field -> Bool
isFieldStrait field = isStrait . pathToCoordinates . path $ field

fix :: Eq a => (a -> a) -> a -> a
fix f a
  | f a == (f . f) a = f a
  | otherwise = fix f (f a)

if' :: Bool -> a -> a -> a
if' True  x _ = x-- 
if' False _ y = y

-- select = foldr (uncurry if') -- base case, list of (condition, expression)

getLines :: IO [String]
getLines = do-- 
  x <- getLine
  if x == ""
    then return []
    else do
      xs <- getLines
      return (x:xs)-- 

takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ [] = []-- 
takeUntil c (x:xs)
        | x == c = []
        | otherwise = x : takeUntil c xs-- 

dropUntil :: Eq a => a -> [a] -> [a]
dropUntil c as = drop (length (takeUntil c as) + 1) as

count :: Eq a => a -> [a] -> Int
count x [] = 0-- 
count x (y:ys) | x==y = 1+(count x ys)
               | otherwise = count x ys-- 

dropUntils :: Eq a => [a] -> [a] -> [a]
dropUntils cs as-- 
  | cslen > length as = []
  | cs == take cslen as = drop cslen as
  | otherwise = dropUntils cs (tail as)
    where cslen = length cs-- 

takeUntils :: Eq a => [a] -> [a] -> [a]
takeUntils cs as = take (length as - length (dropUntils cs as)) as

takeUntils' :: Eq a => [a] -> [a] -> [a]
takeUntils' cs as = take (length as - (length (dropUntils cs as) + length cs)) as

dropUntilN :: Eq a => Int -> a -> [a] -> [a]
dropUntilN 0 _ as = as
dropUntilN n c as = dropUntilN (n-1) c $ dropUntil c as

except :: Eq a => [a] -> a -> [a]
except [] _ = []-- 
except as a
     | a `elem` as = take pos as ++ drop (pos+1) as
     | otherwise = as
       where
            pos = fromJust $ elemIndex a as-- 

without :: Eq a => [a] -> [a] -> [a] -- this is \\
without as [] = as-- 
without [] _ = []
without as (b:bs) = without (if b `elem` as then as `except` b else as) bs-- 

uniques :: Eq a => [a] -> [a]
uniques [] = []-- 
uniques [a] = [a]
uniques (a:as)
      | a `elem` as = a : (uniques (as `except` a))
      | otherwise = a : (uniques as)-- 

uniqueElems :: Eq a => [[a]] -> [[a]]
uniqueElems [] = []-- 
uniqueElems [as] = [as]
uniqueElems (as:ass)
  | (or . map (\as' -> null (as \\ as') && null (as' \\ as)) $ ass) = uniqueElems ass
  | otherwise = as : uniqueElems ass --

uniqueNonSubsetElems :: Eq a => [[a]] -> [[a]]
uniqueNonSubsetElems [] = []-- 
uniqueNonSubsetElems [as] = [as]
uniqueNonSubsetElems (as:ass)
  | (or . map (\as' -> null (as \\ as') || null (as' \\ as)) $ ass) = uniqueNonSubsetElems ass -- here || because it will return true for "abc" and "ba", for example. Needed to eliminate redundant "conflicts".
  | otherwise = as : uniqueNonSubsetElems ass --

startsWithConsonant :: String -> Bool
startsWithConsonant string = head string `elem` ((['b'..'z'] ++ ['B'..'Z']) `without` (['e','i','o','u'] ++ ['E','I','O','U']))

onConsonant :: String -> (String, String) -> String
onConsonant string (y, n) = if startsWithConsonant string then y else n
