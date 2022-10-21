import Hadijatek
import OrderLogic
import SvgFunctions

import Text.Regex.TDFA
import System.Directory
import System.IO
import Data.List
import Data.Maybe
import Data.Matrix as M


fieldToPath :: Lang -> String -> Coordinate -> Field -> String
fieldToPath lang color translate (Field id t name defaultColor root fieldPath) = init (unlines
  [" <path"
  ,"  style=\"" ++ style ++ "\""
  ,"  d=\"" ++ fieldPath ++ "\""
  ,"  transform=\"translate(" ++ show (fst translate) ++ " " ++ show (snd translate) ++ ")\""
  ,"  id=\"path" ++ show id ++ "\" >"
  ,"   " ++ title
  ," </path>"
  ])
    where
      style = "fill:" ++ color ++ ";stroke:#000000;stroke-width:2;stroke-linejoin:round"
      title = "<title>" ++ name ++ "</title>"

fieldAdjsToPaths :: Lang -> Coordinate -> Fields -> BothAdjacencies -> Field -> String
fieldAdjsToPaths lang delta fields badjs field = unlines 
  [fieldToPath lang color translate f
  | f <- sortFieldsByType $ field : neighbors
  , let byWater = f `elem` wneighbors
  , let bySea = f `elem` sneighbors
  , let color = foldr (uncurry if') "#00ff00"
          [(bySea, "#5555ff")
          ,(byWater && fieldType f > 0, "#0033ff")
          ,(byWater, "#0000ff")
          ,(f == field, "#ff0000")]
  , let translate = vecop (-) delta minxy
  ]
  where
    neighbors = nAwayOn fields badjs 2 1 field
    wneighbors = nAwayOn fields badjs 0 1 field
    sneighbors = nAwayOn fields badjs (-1) 1 field
    minxy = (\[x,y] -> (x,y))
          . map minimum 
          . (\(x,y) -> [x,y]) 
          . unzip 
          . concat 
          . map (pathToCoordinates . path)
          $ neighbors

hmapDataToSvg :: Lang -> Fields -> BothAdjacencies -> String
hmapDataToSvg _ [] _ = []-- 
hmapDataToSvg lang fields badjs = unlines ([
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
  "<!-- Created by Atila Schrieber for Hadijáték -->\n",
  "<svg",
  " width=\"" ++ width ++ "\"",
  " height=\"" ++ height ++ "\"",
  " viewbox=\" -1 -1 " ++ width ++ " " ++ height ++ "\"",
  " version=\"1.1\"\n id=\"Map\"\n xml:space=\"preserve\"\n xmlns=\"http://www.w3.org/2000/svg\"\n xmlns:svg=\"http://www.w3.org/2000/svg\"\n style=\"display:inline\" >\n",
  " <!-- " ++ ["Mezők", "Fields"] !! lang ++ " -->"] ++
  fadjPaths) ++
  "</svg>"
    where
      (width, height) = (show 2000, show $ length fields * 1050)
      fadjPaths = 
        [fieldAdjsToPaths lang delta fields badjs field
        | i <- [0..(length fields -1)]
        , let delta = (0.0,1050.0 * fromIntegral i)
        , let field = fields !! i]

main = do
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
  adjs <- readFile "NBMap.adjs"
  let badjs = fetchAdjacencies adjs
  let svg = hmapDataToSvg lang fields badjs
  writeFile ((hmapName =~ ".*\\." :: String) ++ "test.svg") svg-- 

