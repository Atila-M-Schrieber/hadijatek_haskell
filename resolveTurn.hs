import Hadijatek
import OrderLogic

import Text.Regex.TDFA
import System.Directory
import System.IO
import Data.List
import Data.Maybe
import Data.Matrix as M

import qualified Debug.Trace as D
-- import qualified Hood.Observe as D


resolveConflict :: Lang -> Teams -> Fields -> Units -> Orders -> IO Orders
resolveConflict lang teams fields units orders = do -- resolve a single conflict, return orders as "failed" or "unresolved"
  putStrLn $ ["Ezek a parancsok konfliktusban vannak:",
              "The following orders are in conflict:"] !! lang
  putStr . unlines .
    zipWith (++) [show i ++ ": " | i <- [0..(length orders -1)]] .
    map (showOrderPretty lang teams fields units) $ orders
  putStrLn $ ["Válaszd ki, hogy melyik parancsok hiúsulnak meg, a megadott számokkal, vesszőkkel elválasztva (pl.: 0,1 ha a #0 és #1-es parancsok meghiúsultak).",
              "Choose which orders fail, using the numbers given, separated by commas (eg.: 0,1 for orders #0 and #1 failing)."] !! lang
  putStr $ ["Meghiúsult parancsok: ", "Failed orders: "] !! lang
  fails' <- getLine
  let fails = read ('[' : fails' ++ "]") :: [Int]
  let newOrders = -- 
        [order
          | i <- [0..(length orders -1)],
            let status = if i `elem` fails then Failed else Unresolved,
            let o n = orders !! n,
            let order = 
                  Order (orderField $ o i)
                        (orderType $ o i)
                        (orderAffects $ o i)
                        status]-- 
  return newOrders-- 

resolveConflicts :: Lang -> Teams -> Fields -> Units -> Orders -> IO Orders
resolveConflicts lang teams fields units orders = do -- Resolve a round of conflicts, map failed orders, return final orders
  let conflicts = detectConflicts teams fields units orders
  if (not . null) conflicts --
  then do
    putStrLn $ ["A vizsgálat alatt lévő lépések (vedd figyelembe, hogy mely parancsok bukhatnak meg előbb):",
                "These are all orders being presently considered. Consider which ones may fail first."] !! lang
    putStrLn . unlines .
      zipWith (++) [" " | i <- [0..(length orders -1)]] .
      map (showOrderPretty lang teams fields units) $ concat conflicts ++ (orders \\ (concat conflicts))
  else do return () --
  -- let tracedResolveConflict lang teams fields units conflict = trace ("rC" ++ show conflict) (tracedResolveConflict lang teams fields units conflict)
  -- resolved' <- sequence . map (resolveConflict lang teams fields units) $ conflicts -- needs ordering of conflicts
  resolved' <- sequence . map (resolveConflict lang teams fields units) $ conflicts -- needs ordering of conflicts
  let resolved = concat resolved' 
  let mappedOrders = --
        [order
          | i <- [0..(length orders -1)],
            let o = orders !! i,
            let ro = (head . filter (\r -> orderField o == orderField r) $ resolved), -- resolved order
            let order = 
                  if orderField o `elem` map orderField resolved -- if order is conflicting - there is a resolved order with the same starting field
                     then if (orderStatus ro) == Failed -- then return the resolved order, mapped to failed, if needed
                          then mapFailedOrder ro else ro
                     else o ] -- else just the order
  return mappedOrders-- 

resolveAllConflicts :: Lang -> Teams -> Fields -> Units -> Orders -> IO Orders
resolveAllConflicts lang teams fields units orders = do-- 
  resolved <- resolveConflicts lang teams fields units orders
  if intersect orders resolved == orders
  then return resolved
  else resolveAllConflicts lang teams fields units resolved-- 

resolveTurn :: Lang -> Teams -> Fields -> BothAdjacencies -> Units -> Orders -> IO (Teams, Units, [Orders])
resolveTurn lang teams fields badjs units0 orders0 = do -- Return updated teams, fields, units, and a list of orders, first being input orders, the rest being "intermediate" orders, and the last being retrats
  let (units, orders) = applyDeltaOrders teams fields units0 orders0 -- apply get usable units & orders
  resolvedOrders <- resolveAllConflicts lang teams fields units orders -- gets orders which are all successful
  let mappedOrders = map mapSucceededOrder resolvedOrders -- returns applicable orders
  let applicableOrders = map (checkForDeath teams fields badjs units orders) mappedOrders -- finds orders which kill units, make that clear
  let retreatOrders = filter (\o -> orderType o == (-1) && (not . fst . orderAffects $ o)) applicableOrders -- list of retreat orders, will be written to the .retreat (alongside which units to remove)
  let (appliedTeams, appliedUnits) = applyOrders teams fields units applicableOrders
  putStrLn . unlines . map (\o -> "- " ++ showOrderPretty lang teams fields units o) $ orders
  putStrLn . unlines . map (\o -> "->" ++ showOrderPretty lang teams fields units o) $ resolvedOrders
  putStrLn . unlines . map (\o -> "=>" ++ showOrderPretty lang teams fields units o) $ mappedOrders
  putStrLn . unlines . map (\o -> ">>" ++ showOrderPretty lang teams fields units o) $ applicableOrders
  putStrLn . unlines . map (\u -> " " ++ showOrderToActions lang appliedTeams fields appliedUnits (Order (unitField u) 0 (True,[]) Unresolved)) $ appliedUnits
  return (appliedTeams, appliedUnits, [orders, resolvedOrders, mappedOrders, applicableOrders, retreatOrders]) --

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
  (appliedTeams, appliedUnits, allOrders) <- resolveTurn lang teams fields badjs units orders
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
  writeFile retreatsName $ showRetreats appliedTeams fields badjs appliedUnits (head allOrders) (last allOrders) -- head being applied delta orders, last being retreat orders
  genNextOrderFiles name (step +2) teams
  putStrLn $ ["Kész.", "Done."] !! lang --

main = cli
