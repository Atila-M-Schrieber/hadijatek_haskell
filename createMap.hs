import Hadijatek
import SvgFunctions

import Text.Regex.TDFA
import System.Directory
import System.IO
import Data.List
import Data.Matrix as M
import Control.Parallel.Strategies


svgToField :: String -> Int -> String -> String -> String -> Field -- svgContents, fieldID, fieldName, waterColor, baseColor
svgToField svg id name waterColor baseColor = field-- 
  where
       fcolor = takeUntil ';' $ dropUntils "fill:" svg
       fpathcolor = takeUntil ';' $ dropUntils "stroke:" svg
       ftype = if waterColor == fcolor then 0
                 else if baseColor == fpathcolor then 2
                   else 1
       fname = name
       fpath = takeUntil '"' $ dropUntils " d=\"" svg
       froot = (centroid . cullToConvex . removeStrait . pathToCoordinates) fpath
       field = Field id ftype fname fcolor froot fpath-- 

nameAndID :: Field -> String
nameAndID field = -- 
  (if id < 10 then "  " else if id < 100 then " " else "")
  ++ show id ++ ": " ++ fieldName field
    where id = fieldID field-- 

inputTeams :: Lang -> [String] -> Fields -> IO Teams -- lang, names, id, Fields
inputTeams _ [] _ = do return []-- 
inputTeams lang names fields =
  do
    teams <- sequence 
      [do
         putStr $ ["A" ++ onConsonant name ("","z") ++ " " ++ name ++" Csapat színe: ",
                   "Team " ++ name ++ "'s color: "] !! lang
         color <- getLine
         let ids = map fieldID . filter (\field -> color == fieldColor field && 2 == fieldType field) $ fields
         return $ Team id name color $ zip ids $ take (length ids) $ repeat True
       | id <- [0..(length names -1)], let name = names !! id]
    return teams-- 

getFields :: String -> String -> [(Int, (String, String))] -> IO Fields
getFields waterColor baseColor propertiess = do-- 
  fields <- sequence $
              parMap rseq
                (\(id, (path, name)) ->
                  do svg <- readFile path; return $ svgToField svg id name waterColor baseColor)
                propertiess
  return fields --

-- Define different behaviors for using with cli and cgi
-- cgi = do

cli = do
  -- Setup 
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  paths' <- listDirectory "./"
  let paths = sort $ filter (=~ "^.*_[^0-9]*\\.svg$") paths'
  putStr "0: hu\n1: en\nVálassz nyelvet / Pick a language: "
  lang' <- getLine
  let lang = if lang' `elem` ["1", "en"] then 1 else 0
  let name' = (tail . head) (map show paths) =~ "_.*\\.svg\"$" :: (String, String, String)
  name <- if (\(_, field, _) -> field == "") name'-- 
            then do putStr $ ["Térkép neve: ", "Name of map: "] !! lang
                    name'' <- getLine
                    return name''
            else do let name'' = (\(name, _, _) -> name) name'
                    putStrLn $ ["A térkép neve: " ++ name'', "The map will be named: " ++ name''] !! lang
                    return name''-- 
  putStr $ ["Tengeri mezők színe (pl.: #c8ffff): ",
            "Water color you used (eg.: #c8ffff): "] !! lang
  waterColor <- getLine
  putStr $ ["Tengeri mezők körvonalának Színe (pl.: #00a1ff): ",
            "Water Stroke Color (eg.: #00a1ff): "] !! lang
  waterStrokeColor <- getLine
  putStr $ ["Szárazföld Perem Szín: (pl.: #be7145): ",
            "Land Stroke Color (eg.: #be7145): "] !! lang
  landStrokeColor <- getLine
  putStr $ ["Bázissal rendelkező mezők körvonalának színe (pl.: #000000): ",
            "Stroke color you used for fields with bases (eg.: #000000): "] !! lang
  baseColor <- getLine
  -- 

  -- Get fields 
  let propertiess = zip [0..(length paths)] (zip paths (map (takeUntil '.' . dropUntil '_') paths))
  fields <- getFields waterColor baseColor propertiess
  -- Detect bad SVGs
  if foldl (||) False $ map ((elem 'C') . path) fields
    then 
      -- let badFields = filter ((elem 'C') . path) fields -- for some reason this gives a parse error on 'error'
      error $ ["Bèzier görbe van a(z):\n" ++ show (filter ((elem 'C') . path) fields) ++ "\nmező(k)ben.",
               "Bèzier curve detected in:\n" ++ show (filter ((elem 'C') . path) fields) ++ "\nfield(s)."] !! lang
    else putStrLn $ ["Az SVG-k megfelelő formátumban vannak.", "The SVG's are properly formatted."] !! lang-- }}}

  -- Get teams 
  putStrLn $ ["Rögzítsd az összes csapat nevét (enter-rel elválasztva, majd mégegy enter, ha minden csapat megvan): ",
              "Enter all team names (press enter after each one, press enter again when done): "] !! lang
  teamNames <- getLines
  teams <- inputTeams lang (sort teamNames) fields -- 

  -- Adjacencies
  let adjs = getAdjacency fields
  let wadjs = getWaterAdjacency fields

  -- Write file
  let fileName = (name ++ "_0.hmap")
  putStrLn $ ["Információ kiírása a" ++ onConsonant name (" ","z ") ++ fileName ++ " file-ba...",
              "Writing information to " ++ fileName ++ " ..." ] !! lang
  writeFile fileName $ dataToHmap lang 0 name waterStrokeColor landStrokeColor teams fields []
  writeFile (name ++ ".adjs") $ unlines $ map show (M.toLists adjs) ++ "---" : map show (M.toLists wadjs)
  genNextOrderFiles name 1 teams
  putStrLn $ ["Kész.", "Done."] !! lang-- 

main = cli
