import Hadijatek
import SvgFunctions

import Text.Regex.TDFA
import System.Directory
import System.IO
import Data.List
import Data.Maybe
import Data.Matrix as M


fieldToPath :: Lang -> String -> String -> Teams -> Field -> String
fieldToPath lang waterPath landPath teams (Field id t name defaultColor root fieldPath) = init (unlines [-- 
  " <path",
  "  style=\"" ++ style ++ "\"",
  "  d=\"" ++ fieldPath ++ "\"",
  "  id=\"path" ++ show id ++ "\" >",
  "   " ++ title,
  " </path>"])
  ++ straitLines
    where
      occupier = fieldOccupiedBy teams (Right id)
      color = if occupier == Nothing then defaultColor else teamColor $ teams !! (fromJust occupier)
      strokeColor = if t == 0 then waterPath else landPath
      style = "fill:" ++ color ++ ";stroke:" ++ strokeColor ++ ";stroke-width:2;stroke-linejoin:round"
      title = "<title>" ++ fieldTitle lang teams (Field id t name defaultColor root fieldPath) ++ "</title>"
      mslcs = if (isStrait . pathToCoordinates) fieldPath then Just (createStraitLines . pathToCoordinates $ fieldPath) else Nothing
      straitLines = fromJust $ -- 
        if mslcs == Nothing then Just ""
          else
            mslcs >>=
            \slcs -> Just $
              "\n  <line" ++ 
                " x1=\"" ++ show ((fst . fst . fst) slcs) ++
              "\" y1=\"" ++ show ((snd . fst . fst) slcs) ++
              "\" x2=\"" ++ show ((fst . snd . fst) slcs) ++
              "\" y2=\"" ++ show ((snd . snd . fst) slcs) ++
              "\" style=\"stroke:#000000;stroke-width:3;stroke-linecap:round\" >" ++ 
              "<title>" ++ title ++ "</title></line>" ++
              "\n  <line" ++ 
                " x1=\"" ++ show ((fst . fst . snd) slcs) ++
              "\" y1=\"" ++ show ((snd . fst . snd) slcs) ++
              "\" x2=\"" ++ show ((fst . snd . snd) slcs) ++
              "\" y2=\"" ++ show ((snd . snd . snd) slcs) ++
              "\" style=\"stroke:#000000;stroke-width:3;stroke-linecap:round\" >" ++
              "<title>" ++ title ++ "</title></line>"-- }}}

unitToPath :: Lang -> Fields -> Teams -> (Float, Float, Float) -> Unit -> String -- svg line
unitToPath lang fields teams (smallArea, minSize, maxSize) (Unit field team t) = -- 
 case t of
  0 -> "<polygon points=\"" ++
       toSvgPoints [(0,-1),(s32, 0.5),(-s32, 0.5)] ++
       "\" style=\"" ++ style ++ "\">" ++ title ++
       "</polygon>" 
  1 -> "<circle cx=\"" ++
       (show . fst) pos ++ "\" cy=\"" ++ (show . snd) pos ++ "\" r=\"" ++ show (0.75 * size) ++
       "\" style=\"" ++ style ++ "\">" ++ title ++
       "</circle>" 
  2 -> "<rect x=\"" ++
       show (fst pos - 0.7 * size) ++ "\" y=\"" ++ show (snd pos - 0.7 * size) ++ "\" width=\"" ++ show (1.4 * size)  ++ "\" height=\"" ++ show (1.4 * size) ++
       "\" style=\"" ++ style ++ "\">" ++ title ++
       "</rect>" 
  3 -> "<polygon points=\"" ++
       toSvgPoints supertankPoints ++
       "\" style=\"" ++ style ++ "\">" ++ title ++
       "</polygon>" 
  4 -> "<rect x=\"" ++
       show (fst pos - 6/5 * size) ++ "\" y=\"" ++ show (snd pos - 2/5 * size) ++ "\" width=\"" ++ show (12/5 * size)  ++ "\" height=\"" ++ show (4/5 * size) ++ "\" rx=\"" ++ show (2/5 * size) ++
       "\" style=\"" ++ style ++ "\">" ++ title ++
       "</rect>" 
  5 -> "<polygon points=\"" ++
       (toSvgPoints $ map (vecop (*) (0.9,0.9)) [(-1,0),(-0.5,s32),(0.5,s32),(1,0),(0.5,-s32),(-0.5,-s32)]) ++
       "\" style=\"" ++ style ++ "\">" ++ title ++
       "</polygon>" 
 where-- 
   color = teamColor $ teams !! team
   style = "fill:" ++ color ++ ";stroke:#000000;stroke-width:2;stroke-linejoin:miter"
   pos = fieldRoot $ fields !! field
   size = if smallArea >= (abs . area . pathToCoordinates . path $ fields !! field) then minSize else maxSize
   toSvgPoint = (:) ' ' . init . tail . show . vecop (+) pos . vecop (*) (size, size)
   toSvgPoints = tail . concat . map toSvgPoint
   s32 = sqrt 3 / 2
   supertankPoints' = [(0,-1),(-s32, 0.5),(2*s32, 0.5),(s32,-1),(s32/2,-0.25)]
   supertankPoints = map (\c -> vecop (-) c (vecop (+) (0,0.1) $ centroid supertankPoints')) supertankPoints'
   title = -- 
     "<title>" ++
     fieldTitle lang teams (fields !! field) ++ "&#10;" ++
     teamName (teams !! team) ++ " " ++
     unitName lang (Right t) ++
     "</title>"-- }}}}}}

basesToDots :: Lang -> Teams -> Fields -> String
basesToDots lang teams fields = -- 
  unlines 
  [if isHomeBase
    then " <path style=\"stroke:#000000;stroke-width:2.5\" " ++
         "d=\" M" ++ show (fst root-3.5) ++ "," ++ show (snd root-3.5) ++
           " " ++ show (fst root+3.5) ++ "," ++ show (snd root+3.5) ++
          " M " ++ show (fst root+3.5) ++ "," ++ show (snd root-3.5) ++
           " " ++ show (fst root-3.5) ++ "," ++ show (snd root+3.5) ++ "\" >" ++
         "<title>" ++ title ++ "</title></path>"
    else " <circle cx=\"" ++ show (fst root) ++ "\" cy=\"" ++ show (snd root) ++
         "\" r=\"3.5\" ><title>" ++ title ++ "</title></circle>"
   | field <- fields,
     fieldType field == 2,
     let isHomeBase = any snd . filter (\b -> fst b == fieldID field) . concat . map baseIDs $ teams,
     let root = fieldRoot field,
     let title = fieldTitle lang teams field]-- 

hmapDataToSvg :: Lang -> String -> String -> Teams -> Fields -> Units -> String
hmapDataToSvg _ _ _ _ [] _ = []-- 
hmapDataToSvg lang waterPath landPath teams fields units = unlines (
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
  , "<!-- Created by Atila Schrieber for Hadijáték -->\n"
  , "<svg"
  , " width=\"" ++ width ++ "\""
  , " height=\"" ++ height ++ "\""
  , " viewbox=\" -1 -1 " ++ width ++ " " ++ height ++ "\""
  , " version=\"1.1\"\n id=\"Map\"\n xml:space=\"preserve\"\n xmlns=\"http://www.w3.org/2000/svg\"\n xmlns:svg=\"http://www.w3.org/2000/svg\"\n style=\"display:inline\" >\n"
  , " <!-- " ++ ["Háttér", "Background"] !! lang ++ " -->"
  , " <rect x=\"-1\" y=\"-1\" width=\"" ++ width ++ "\" height=\"" ++ height ++ "\" style=\"fill:#555555\" />"
  , ""
  , " <!-- " ++ ["Mezők", "Fields"] !! lang ++ " -->"
  ] ++
  map (fieldToPath lang waterPath landPath teams) (sortFieldsByType fields)) ++
  "\n <!-- " ++ ["Egységek", "Units"] !! lang ++ " -->\n" ++
  unlines (map (unitToPath lang fields teams (2/3 * avgSize, 15, 20)) units) ++
  "\n <!-- " ++ ["Bázisok", "Bases"] !! lang ++ " -->\n" ++
  basesToDots lang teams fields ++
  "</svg>"
    where
      dimension nth = maximum $ map (maximum . map nth . pathToCoordinates . path) fields
      width = show $ dimension fst
      height = show $ dimension snd
      avgSize =
        (\as -> realToFrac (sum as) / genericLength as) .
        map (\(Field _ _ _ _ _ p) -> abs . area . pathToCoordinates $ p) .
        filter (\f -> fieldType f /= 0) $ fields-- 


cli = do
  -- Setup & user input
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  paths' <- listDirectory "./"
  let paths = sort $ filter (=~ "\\.hmap$") paths'
  putStr "0: hu\n1: en\nVálassz nyelvet / Pick a language: "
  lang' <- getLine
  let lang = if lang' `elem` ["1", "en"] then 1 else 0
  putStrLn $ ["Válassz egy térképet: ",
              "Select a map: "] !! lang
  putStr $ unlines [show i ++ ": " ++ show (paths !! i) | i <- [0..(length paths -1)]]
  putStr $ ["Térkép: ", "Map: "] !! lang
  hmapID <- getLine-- 

  -- Process data
  let hmapName = paths !! (read hmapID :: Int)
  hmap <- readFile hmapName
  let teams = fetchTeams hmap
  let fields = fetchFields hmap
  let units = fetchUnits hmap
  let waterPath = dropUntils ": " $ lines hmap !! 2
  let landPath = dropUntils ": " $ lines hmap !! 3
  let svg = hmapDataToSvg lang waterPath landPath teams fields units
  writeFile ((hmapName =~ ".*\\." :: String) ++ "svg") svg-- 

main = cli
