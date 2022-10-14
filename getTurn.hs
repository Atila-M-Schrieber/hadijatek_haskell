{-# LANGUAGE ParallelListComp #-}

import Hadijatek
import OrderLogic

import Text.Regex.TDFA
import System.Directory
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import Network.CGI
import Text.XHtml
import GHC.IO.Encoding
import Control.Exception
import qualified System.IO.Strict as S

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)


type Passons = [(String, Maybe String)] -- form stuff that needs to be passed on, eg passwd


hashPassword :: Int -> Fields -> String -> String
hashPassword tid fields passwd = show . md5 . pack $ show tid ++ passwd ++ fieldName (fields !! tid)

page :: String -> Html -> Html
page t b = header << thetitle << t +++ body << b

rejectBadPassword :: Lang -> [(Int,String)] -> Fields -> Maybe String -> Maybe String -> Html -> Html
rejectBadPassword lang auths fields mtid mpasswd normalHtml = -- 
  if (tid, hashPassword tid fields passwd) `elem` auths
    then normalHtml
    else page (["Rossz jelszó", "Wrong password"] !! lang) $
      paragraph << ["Rossz jelszó! Nyomd a \"vissza\" gombot, hogy újra próbáld!", "Wrong password! Press \"back\" to try again."] !! lang
  where
    tid = read (fromJust mtid) :: Int
    passwd = fromJust mpasswd --

resetButton :: Lang -> Passons -> Html
resetButton lang passons =
  br +++ br +++ br +++ 
  form ! [method "post"] << 
    (passonsToHiddens passons ++ -- teamID & passwd
    [hidden "reset" "True"
    ,(submit ! [thestyle "color:red"]) "" (["Újrakezdés", "Reset"] !! lang)])

resetPage :: Lang -> Maybe String -> Maybe String -> Maybe String -> IO Html
resetPage lang mtid mturnPath mreset = do
  clearTemps "actiontemp"
  clearTemps "targettemp"
  clearTemps "affecttemp"
  clearOrders
  return $ page (["Újrakezdés", "Reset"] !! lang) body
  where
    tid = read (fromJust mtid) :: Int
    tid' = maybe "" id mtid
    clearTemps ss = when (isJust mreset) $ writeFile (ss ++ tid') ""
    clearOrders = when (isJust mreset && isJust mturnPath) $ writeFile (fromJust mturnPath) ""
    body = 
      paragraph << ["Adatok törölve.", "Data reset."] !! lang +++
      form << submit "" (["Tovább", "Next"] !! lang)

passonsToHiddens :: Passons -> [Html]
passonsToHiddens passons = plist  -- ONLY USE ON KNOWN-TO-EXIST VALUES
  where
    passonToHidden (name, value) = hidden name (fromJust value) 
    plist = [passonToHidden passon
            | passon <- passons
            , isJust (snd passon)]--

maybeWriteNewOrder :: Maybe FilePath -> Orders -> Maybe Order -> IO()
maybeWriteNewOrder mturnPath orders mnewOrder = -- 
  maybe
    (do return () )
    (\turnPath ->
      do maybe
          (writeFile turnPath $ unlines . map orderToEntry $ orders) -- If starting, clear the file, put orders in
          (\o -> appendFile turnPath $ orderToEntry o ++ "\n") -- else append the file with the order
          mnewOrder 
         return () )
    mturnPath --

writeAspect :: Show a => String -> [a] -> IO()
writeAspect file list = do
  unless (null list) $ 
    writeFile file $ show list -- write list of stuff to end of file, to be zipped into .turn later
  return ()

loginPage :: Lang -> Teams -> Html
loginPage lang teams = page (["Lépés Bejelentkezés", "Turn Login"] !! lang) $ -- 
  form ! [method "post"] << -- use select menu of team names? prlly better
    [paragraph << (["Csapat: ", "Team: "] !! lang +++ (select ! [name "teamID"] << teamOpts))
    ,paragraph << (["Jelszó: ", "Password: "] !! lang +++ (password ! [value "default1"]) "passwd") -- REMOVE DEFAULT
    ,hidden "clearTemps" "True"
    ,submit "" (["Tovább", "Next"] !! lang) ]
  where
    teamOpts = teams `seq` map (\t -> option ! [value . show $ teamID t] << teamName t) $ teams --

newUnitsPage :: Lang -> Passons -> Maybe String -> Maybe String -> Maybe FilePath -> Int -> Teams -> Fields -> Units -> Maybe String -> Orders -> IO Html
newUnitsPage lang passons mField mType mturnPath step teams fields units mtid orders = do-- DOES NOT DO REMOVAL YET
  maybeWriteNewOrder mturnPath orders mnewOrder 
  return $ page title body
  where
    tid = read (fromJust mtid) :: Int
    team = teams !! tid
    -- turnPath = fromJust mturnPath -- should be Just by now
    mnewOrder = do --
      fid' <- mField
      let fid = read fid' :: Int
      t' <- mType
      let t = read t' :: Int
      return $ Order fid 5 (True, [t]) Unresolved --
    orders' = if isNothing mnewOrder then orders else (fromJust mnewOrder) : orders
    units' = fst $ applyDeltaOrders teams fields units orders'
    title = ["Új egységek - " ++ teamName team, "New units - " ++ teamName team] !! lang
    submittedNewUnits = foldl (vecop' (+)) (0,0) -- -- not needed
      [(1, super)
      | let dorders = filter (\o -> orderType o == 5) orders'
      , dorder <- dorders
      , let super = if (head . snd . orderAffects $ dorder) `elem` [2..5] then 1 else 0
      ] --
    (dnormal, dsuper) = howManyNewUnits step team units' -- using units' eliminates need to re-check unitField collisions
    fieldRadios = -- 
      [label << ((radio ! chkd) "newUField" (show fid) +++ (toHtml . fieldName $ fields !! fid))
      | let fhbs = freeHomeBases team units'
      , i <- [0..(length fhbs -1)]
      , let chkd = if i == 0 then [checked] else []
      , let fid = fhbs !! i] --
    unitOpts = --
      [option ! [value . show $ t] << unitName lang (Right t) -- no check for putting water unit on landlocked field
      | let opts = 0 : 1 : if dsuper > 0 then [2..5] else []
      , t <- opts
      ] -- 
    newUnitSelection = form ! [method "post"] << --
      (passonsToHiddens passons ++ 
      [paragraph << fieldRadios
      ,paragraph << (select ! [name "newUType"] << unitOpts)
      ,submit "" (["Tovább", "Next"] !! lang) ]) --
    body = if dnormal > 0 --
           then paragraph << ["Hova állítasz fel egységet?", "Where will you place your new unit?"] !! lang +++ newUnitSelection
           else paragraph << ["Nem állíthatsz több egységet", "You cannot place any more units"] !! lang +++ 
             form ! [method "post"] << 
               (passonsToHiddens passons ++ -- teamID & passwd
               [hidden "newUnitOrdersDone" "True"
               ,hidden "clearTemps" "True"
               ,submit "" (["Tovább", "Next"] !! lang) ]) --}}}

ordersPage :: Lang -> Passons -> Maybe String -> Teams -> Fields -> Units -> Maybe String -> Orders -> Int ->  [(Int,Int)] -> [(Int,Int)] -> [(Int,(Bool, [Int]))] -> IO Html
ordersPage lang passons mclear teams fields units' mtid orders progress actions targets affects = do -- gets actions of units
  clearTemps (hasBeenSubmitted actions) "actiontemp"
  clearTemps (hasBeenSubmitted targets) "targettemp"
  clearTemps (hasBeenSubmitted affects) "affecttemp"
  unless (progress == 0) $ 
    case progress of
      1 -> writeFile ("actiontemp" ++ show tid) $ show actions
      2 -> writeFile ("targettemp" ++ show tid) $ show targets
      3 -> writeFile ("affecttemp" ++ show tid) $ show affects
  return $ page title body -- gets actions of orders
  where
    tid = read (fromJust mtid) :: Int
    tid' = maybe "" id mtid
    clearTemps b ss = when (isJust mclear || b) $ writeFile (ss ++ tid') ""
    hasBeenSubmitted as = length as > length units -- clears if "going back"
    team = teams !! tid
    units = filter (\u -> unitTeam u == tid) . fst $ applyDeltaOrders teams fields units' orders -- Applies any delta orders, so they can be commanded, and filters for team's units

    -- Find action, target, and affects - only use when progress is there
    find' unit = snd . head . filter (\a -> fst a == unitField unit)
    findAction unit = find' unit actions
    findTarget unit = find' unit targets
    findAffect unit = find' unit affects

    -- Selection available to unit  
    defaultSelect unit p = select ! [name $ p ++ show (unitField unit)]
    actionSelect unit = --
      defaultSelect unit "action" <<
        [option ! [value . show $ ot] << orderName lang (Right ot)
        | ot <- if unitType unit == 5 then [0..4] else [0..3] ] --
    -- targetSelect unit (based off actions)
    
    targetSelect unit = --
      case findAction unit of
        1 -> hidden ("target" ++ show (unitField unit)) . show $ unitField unit -- defending then targeting itself
        3 -> defaultSelect unit "target" << -- "target" is unit to transform into
               [option ! [value . show $ uid] << unitName lang (Right uid)
               | uid <- [0..5]
               , uid /= unitType unit ]
        _ -> defaultSelect unit "target" <<
               [option ! [value . show . fieldID $ field] << fieldName field
               | field <- fields] -- getTargetableFields fields (findAction unit) unit] -- works, but slow, just check legality @ end
    -- affectSelect unit (based off actions & targets)
    conq unit = "conq" ++ show (unitField unit)
    teamSelect t unit = 
      [label ! [thefor $ iD ++ teamName team] << (" - " ++ teamName team) +++
       (t ! chkd) iD (show . teamID $ team)
      | team <- teams
      , let iD = "affect" ++ show (unitField unit)
      , let ident = identifier (iD ++ teamName team)
      , let chkd = if teamID team == tid then [checked, ident] else [ident]
      ] +++
      hidden (conq unit) "False"
    affectSelect unit = --
      case findAction unit of
        0 -> (if fields !! (findTarget unit) `elem` nAwayOn fields 2 1 (fields !! unitField unit) || unitField unit == findTarget unit -- (conquer in place)
              then noHtml
              else defaultSelect unit "affect" <<
                [option ! [value . show . fieldID $ field] << fieldName field
                 | field <- fields])
                {-defaultSelect unit "affect" << -- does not work???
                  [option ! [value . show .fieldID $ field] << fieldName field
                  | field <- fields
                  , fieldsAreNeighbors field (fields !! unitField unit) -- neighbors starting field
                  , fieldsAreNeighbors field (fields !! findTarget unit) -- and the target
                  ]
             -}
             +++
             label ! [thefor $ conq unit ++ "T"] << (" - " ++ ["Foglal", "Conquers"] !! lang) +++
             (radio ! [checked, identifier (conq unit ++ "T")]) (conq unit) "True" +++
             label ! [thefor $ conq unit ++ "F"] << [" Nem foglal", " Does not conquer"] !! lang +++
             (radio ! [identifier (conq unit ++ "F")]) (conq unit) "False" 
        1 -> teamSelect checkbox unit
        2 -> teamSelect radio unit
        _ -> const noHtml unit --

    order unit = Order (unitField unit) (findAction unit) ((fst . findAffect $ unit), (snd . findAffect $ unit) ++ [findTarget unit]) Unresolved
    
    (dname, tname, selectPrefix, aspectSelect) = 
      case progress of
        0 -> ("action"
             , ["Akciók","Actions"] !! lang
             , \unit -> showOrderToActions lang teams fields units $ 
                Order (unitField unit) 0 (True,[]) Unresolved
             , actionSelect)
        1 -> ("target"
             , ["Célmezők","Targets"] !! lang
             , \unit -> showOrderToTargets lang teams fields units $
                Order (unitField unit) (findAction unit) (True,[]) Unresolved
             , targetSelect)
        2 -> ("affect"
             , ["Hatáskör","Affects"] !! lang
             , \unit -> showOrderToAffects lang teams fields units $
                Order (unitField unit) (findAction unit) (True, [findTarget unit]) Unresolved
             , affectSelect)
        3 -> ("ordersDone"
             , ["Ellenőrzés","Confirmation"] !! lang
             , showOrderPretty lang teams fields units . order 
             , const noHtml)

    formElem unit = paragraph << [toHtml $ selectPrefix unit ++ " ", aspectSelect unit]
    -- iOL = isOrderLegal fields units . order
    iOL = const True -- Not working, fix
    illegalStyle unit = if iOL unit then [] else [thestyle "color:red"]
    illegalPrefix unit = if iOL unit then "" else ["ILLEGÁLIS - ", "ILLEGAL - "] !! lang
    doneElem unit = paragraph ! illegalStyle unit <<
      [toHtml $ illegalPrefix unit ++ selectPrefix unit ++ " ", aspectSelect unit]
    -- Orders: (part)
    title = ["Parancsok: " ++ tname ++ " - " ++ teamName team
            , "Orders: " ++ tname ++ " - " ++ teamName team] !! lang
    -- do case prlly
    body =
      if progress /= 3 
      then
        paragraph << (["Válassz, a parancsaidnak megfelelően", "Make selections based on your orders."] !! lang ) +++ --
        form ! [method "post"] <<
          (passonsToHiddens passons ++ -- teamID & passwd, newUnitOrdersDone
           map formElem units ++
          [hidden dname "done"
          ,submit "" (["Tovább", "Next"] !! lang)] )
      else
        -- if all (isOrderLegal fields units . order) units
        -- then
        paragraph << ["Ellenőrizd a parancsaidat", "Confirm your orders"] !! lang +++ --
        form ! [method "post"] <<
          (passonsToHiddens passons ++ -- teamID & passwd, newUnitOrdersDone
           map doneElem units ++ -- Just shows orders
          [ hidden dname "done"
          -- , button ! [value "restart"] << (["Újrakezdés", "Restart"] !! lang)
          , submit "" (["Lead", "Submit"] !! lang)])
        -- else
        -- paragraph << ["Szabálytalan parancs észlelve", "Illegal order detected"] !! lang +++
        -- map doneElem units

submitPage :: Lang -> Maybe String -> Maybe String -> Maybe String -> Teams -> Fields -> Units -> Orders -> [(Int,Int)] -> [(Int,Int)] -> [(Int,(Bool, [Int]))] -> IO Html
submitPage lang mturnPath mordersDone mtid teams fields units' orders actions targets affects = do
  clearTemps "actiontemp"
  clearTemps "targettemp"
  clearTemps "affecttemp"
  when (isJust mordersDone)
    . appendFile (fromJust mturnPath)
    . unlines . map (orderToEntry . newOrder) $ units
  return $ page title body
  where
    tid = read (fromJust mtid) :: Int
    tid' = maybe "" id mtid
    clearTemps ss = when (isJust mordersDone) $ writeFile (ss ++ tid') ""
    units = filter (\u -> unitTeam u == tid) . fst $ applyDeltaOrders teams fields units' orders -- Applies any delta orders, so they can be commanded, and filters for team's units

    find' unit = snd . head . filter (\a -> fst a == unitField unit)
    findAction unit = find' unit actions
    findTarget unit = find' unit targets
    findAffect unit = find' unit affects

    newOrder unit = Order (unitField unit) (findAction unit) ((fst . findAffect $ unit), (snd . findAffect $ unit) ++ [findTarget unit]) Unresolved

    title = ["Parancsok leadva", "Orders submitted"] !! lang
    body = 
      paragraph << ["Parancsok leadva", "Orders submitted"] !! lang +++
      paragraph << (unlines . map (show . findAffect) $ units) +++
      paragraph << (show units) +++
      paragraph << (show affects) +++
      paragraph << (unlines . map (show . newOrder) $ units) +++
      form ! [method "post"] << 
        [hidden "clearTemps" "True"
        ,submit "" (["Kész", "Done"] !! lang) ] --}}}

placeholderPage :: Html
placeholderPage = page "PLACEHOLDER" $ paragraph << "PLACEHOLDER"

getNewestMap :: [FilePath] -> IO String -- Retuns newest hmap
getNewestMap mapPaths = do-- 
  let numPaths =
        zip (map (\p -> read (init ((p :: String) =~ "[0-9]*\\.") :: String) :: Int) mapPaths)
            mapPaths
  let newestPath = 
        snd . last . sortBy (\np1 np2 -> compare (fst np1) (fst np1)) $ numPaths
  hmap <- readFile newestPath
  return $! hmap --

maybeFetchOrders :: Maybe FilePath -> IO Orders
maybeFetchOrders mturnPath = do-- 
  maybe 
    (return $! [] :: IO Orders)
    (\path -> S.readFile path >>= (\turn -> return $! fetchOrders turn))
    mturnPath --


cgiMain = do
  -- General setup
  liftIO $ setLocaleEncoding utf8
  paths' <- liftIO $ listDirectory "./"
  let mapPaths = sort $ filter (=~ "\\.hmap$") paths'
  hmap <- liftIO $ getNewestMap mapPaths
  let teams = fetchTeams hmap
  let fields = fetchFields hmap
  let units = fetchUnits hmap
  let name = takeUntil '\n' . dropUntil ' ' $ hmap
  auths' <- liftIO . readFile $ name ++ ".passwords"
  let auths = read auths' :: [(Int,String)]
  let step = (read (takeUntil '\n' . dropUntilN 2 ' ' $ hmap) :: Int) + 1
  mlang <- getInput "lang"
  let lang = maybe 0 (\l -> if l `elem` ["1", "en"] then 1 else 0) mlang --

  -- All Inputs
  allInputs <- getInputs -- for when Maybe isn't the important bit
  -- Authentication 
  tid <- getInput "teamID"
  passwd <- getInput "passwd"

  -- New Units
  newUType <- getInput "newUType"
  newUField <- getInput "newUField"
  newUnitOrdersDone <- getInput "newUnitOrdersDone"
  action <- getInput "action"
  target <- getInput "target"
  affect <- getInput "affect"

  -- Orders
  clearTemps <- getInput "clearTemps"
  let mtid = maybe "" id tid
  actions' <- liftIO $ S.readFile $ "actiontemp" ++ mtid
  targets' <- liftIO $ S.readFile $ "targettemp" ++ mtid
  affects' <- liftIO $ S.readFile $ "affecttemp" ++ mtid
  let actions = [(f, t) --
                | input <- allInputs
                , fst input =~ "action.+"
                , let f = read (dropUntils "action" . fst $ input) :: Int
                , let t = read (snd input) :: Int] ++
                if null actions' then [] else read actions' :: [(Int,Int)] --
  let targets = [(f, t) --
                | input <- allInputs
                , fst input =~ "target.+"
                , let f = read (dropUntils "target" . fst $ input) :: Int
                , let t = read (snd input) :: Int] ++
                if null targets' then [] else read targets' :: [(Int,Int)] --
  let affects = [(f, (b, as)) --
                | input <- allInputs
                , fst input =~ "conq.+"
                , let f = read (dropUntils "conq" . fst $ input) :: Int
                , let b = read (snd input) :: Bool
                , let as' = filter (\a -> fst a =~ ("affect" ++ show f)) allInputs
                , let as = map (\a -> read (snd a) :: Int) as'] ++
                if null affects' then [] else read affects' :: [(Int,(Bool, [Int]))] --
  let progress = length . filter (not . null) $ [actions, targets, map (\_ -> (0,0)) affects]
  ordersDone <- getInput "ordersDone"
  mreset <- getInput "reset"

  -- Diagnostics
  diagnostics <- getInput "diagnostics"

  -- Get turns
  let mturnPath = maybe
        Nothing
        (\tid' -> Just . head $ filter (=~ (name ++ "_" ++ show step ++ "_" ++ tid' ++ "\\.turn$")) paths')
        tid
  orders <- liftIO . maybeFetchOrders $ mturnPath -- avoids lists being read as orders

  -- Number coding for which from to show. KEEP PASSING ON (needed) PREVIOUS INPUTS
  let code = fromJust $ foldM (\lastCode (mInput, code) -> if isNothing mInput then Just lastCode else Just code)
        0 -- If username / password is not supplied, be on login page
        [(tid,1),(passwd,1) -- If newUnitOrders are not fully supplied, be on newUnit page
        ,(newUnitOrdersDone,2)
        ,(ordersDone,3)
        ,(mreset,4)] -- Etc.

  -- The html output
  let passons = [("passwd", passwd),("teamID",tid)]
  newUPage <- liftIO $ newUnitsPage lang [("passwd", passwd),("teamID",tid)] newUField newUType mturnPath step teams fields units tid orders
  let passons' = ("newUnitOrdersDone",newUnitOrdersDone) : passons
  -- oPage <- liftIO $ progress `seq` ordersPage lang passons' teams fields units tid orders progress actions targets affects
  oPage <- liftIO $ ordersPage lang passons' clearTemps teams fields units tid orders progress actions targets affects
  subPage <- liftIO $ submitPage lang mturnPath ordersDone tid teams fields units orders actions targets affects
  rPage <- liftIO $ resetPage lang tid mturnPath mreset
  let pages = 
        loginPage lang teams : 
        map (\page -> rejectBadPassword lang auths fields tid passwd page +++ resetButton lang passons)
          [newUPage
          ,oPage
          ,subPage]
        ++ [rPage] -- no passons, so fails on rejectBadPassword
  let diags = if isJust diagnostics
              then paragraph <<
                (show mturnPath ++ " " ++ show code
                ++ show actions ++ show targets ++ show affects
                ++ " , " ++ show progress ++ "\n" ++ show allInputs)
              else noHtml

  output . renderHtml $ diags +++ pages !! code 

main = runCGI $ handleErrors cgiMain
