import Hadijatek
import OrderLogic

import Text.Regex.TDFA
import System.Directory
import System.IO
import Data.List
import Data.Maybe
import Data.Matrix as M

autoResolveConflict :: Teams -> Fields -> BothAdjacencies -> Units -> Orders -> Orders
autoResolveConflict teams fields badjs units conflict = -- Resolve single conflict (Orders input = single conflict)
  []

autoResolveConflicts :: Teams -> Fields -> BothAdjacencies -> Units -> Orders -> Orders
autoResolveConflicts teams fields badjs units orders  -- Resolve all conflicts, until none are left
  | done = resolved
  | otherwise = autoResolveConflicts teams fields badjs units newOrders
  where
    conflicts = detectConficts' teams fields units orders
    done = null conflicts
    resolved = autoResolveConflict teams fields badjs units $ head conflicts
    newOrders = --
      [order
      | i <- [0..(length orders -1)]
      ,   let o = orders !! i
      ,   let ro = (head . filter (\r -> orderField o == orderField r) $ resolved) -- resolved order
      ,   let order = 
                if orderField o `elem` map orderField resolved -- if order is conflicting - there is a resolved order with the same starting field
                   then if (orderStatus ro) == Failed -- then return the resolved order, mapped to failed, if needed
                        then mapFailedOrder ro else ro
                   else o -- else just the order
      ] 

--newOrders being whatever happens after single resolved conflict

autoResolveTurn :: Teams -> Fields -> BothAdjacencies -> Units -> Orders -> (Teams, Units, Orders)
autoResolveTurn teams fields badjs units0 orders0 = -- Return updated teams, fields, and units, and retreats
  (doneTeams, doneUnits, retreats)
  where 
    (units, orders) = applyDeltaOrders teams fields units0 orders0 -- apply get usable units & orders
    resolvedOrders = autoResolveConflicts teams fields badjs units orders
    mappedOrders = map mapSucceededOrder resolvedOrders -- returns applicable orders
    applicableOrders = map (checkForDeath teams fields badjs units orders) mappedOrders -- finds orders which kill units, make that clear
    (doneTeams, doneUnits) = applyOrders teams fields units resolvedOrders
    retreats = filter (\o -> orderType o == (-1) && (not . fst . orderAffects $ o)) applicableOrders -- list of retreat orders, will be written to the .retreat (alongside which units to remove)

showRetreats :: Teams -> Fields -> BothAdjacencies -> Units -> Orders -> Orders -> String
showRetreats _ _ _ _ _ [] = []-- 
showRetreats teams fields badjs units orders (retreat:retreats) = 
  (unlines . map orderToEntry $ possibleRetreats teams fields badjs units orders retreat)
  ++ showRetreats teams fields badjs units orders retreats --


cli = do
  -- Setup & user input
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  paths' <- listDirectory "./"
  let mapPaths = sort $ filter (=~ "\\.hmap$") paths'
  putStr "0: hu\n1: en\nVálassz nyelvet / Pick a language: "
  lang' <- getLine
  let lang = if lang' `elem` ["1", "en"] then 1 else 0
  putStrLn $ ["Válassz egy térképet: ",
              "Select a map: "] !! lang
  putStr $ unlines [show i ++ ": " ++ show (mapPaths !! i) | i <- [0..(length mapPaths -1)]]
  putStr $ ["Térkép: ", "Map: "] !! lang
  hmapID <- getLine-- 

  -- Fetch data
  let hmapName = mapPaths !! (read hmapID :: Int)
  hmap <- readFile hmapName
  let teams = fetchTeams hmap
  let fields = fetchFields hmap
  let units = fetchUnits hmap
  let name = takeUntil '\n' . dropUntil ' ' $ hmap
  adjs <- readFile $ name ++ ".adjs"
  let badjs = fetchAdjacencies adjs
  let step = read (takeUntil '\n' . dropUntilN 2 ' ' $ hmap) :: Int
  let turnPaths = sort $ filter (=~ (name ++ "_" ++ show (step +1) ++ "_[0-9].*" ++ "\\.turn$")) paths'
  turns' <- sequence . map readFile $ turnPaths
  let orders = concat . map fetchOrders $ turns'

  -- application
  let (appliedTeams, appliedUnits, retreats) = autoResolveTurn teams fields badjs units orders
  let nextHmapName' = hmapName =~ "_[0-9].*\\." :: (String, String, String)
  let nextHmapName = fst' nextHmapName' ++ "_" ++ show (step +1) ++ ".hmap"
  let waterPath = dropUntils ": " $ lines hmap !! 2
  let landPath = dropUntils ": " $ lines hmap !! 3
  putStrLn $ ["Térkép új állásának kiírása " ++ nextHmapName ++ " -ba...",
              "Writing new map state to " ++ nextHmapName ++ " ..."] !! lang
  writeFile nextHmapName
    $ dataToHmap lang (step +1) name waterPath landPath appliedTeams fields appliedUnits
  let retreatsName = fst' nextHmapName' ++ "_" ++ show (step +1) ++ ".retreats"
  putStrLn $ ["Visszavonulások kiírása " ++ retreatsName ++ " -ba...",
              "Writing retreats to " ++ retreatsName ++ " ..."] !! lang
  writeFile retreatsName $ showRetreats appliedTeams fields badjs appliedUnits (snd $ applyDeltaOrders teams fields units orders) retreats -- head being applied delta orders, last being retreat orders
  genNextOrderFiles name (step +2) teams
  putStrLn $ ["Kész.", "Done."] !! lang --

main = cli
