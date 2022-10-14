module OrderLogic where
import Hadijatek

import Data.List
import Data.Maybe
import Data.Matrix as M


nUnits :: Units -> Team -> (Int, Int) -- (Total units, super units)
nUnits units team = (length teamUnits, length superUnits)-- 
  where
    teamUnits = filter (\unit -> unitTeam unit == teamID team) units
    superUnits = filter (\unit -> unitType unit >= 2) teamUnits-- 

unitCapacity :: Team -> (Int, Int) -- (Total unit capacity, super capacity)
unitCapacity team = (length bases, superCapacity)-- 
 where
   bases = baseIDs team
   superCapacity = length bases `div` 3 + if (length . filter snd $ bases) == 3 then 1 else 0-- 

transformLegal :: Teams -> Units -> Order -> Bool -- CHECK ONCE ALL CONFLICTS RESOLVED Checks if super capacity wil be exceeded if transformation happens
transformLegal teams units order = -- 
  if (head . snd . orderAffects) order >= 2
    then
      if mteam == Nothing then False
        else (snd $ nUnits units $ fromJust mteam) < (snd $ unitCapacity $ fromJust mteam)
    else True
  where mteam = teamOfOrder teams units order-- 

adjRowToIDs :: [Int] -> [Int] -- Takes row of adjacency matrix, returns id's of fields which are "adjacent"
adjRowToIDs row = [n | n <- [0..(length row -1)], row !! n > 0]

-- Returns unique fields which are n fields away, based on given adjacency map
nAway :: Fields -> Adjacencies -> Int -> Field -> Fields
nAway fields adjs n field = uniques-- 
  [field'
    | field' <- fields `except` field,
      let row = toLists (matrixNth adjs n) !! fieldID field,
      fieldID field' `elem` adjRowToIDs row]-- 

-- nAway limited to fields limited to sea (t==(-1)), water (t==0), or land (t==1) -type movement, or (t==2) for everything (air)
nAwayOn :: Fields -> Int -> Int -> Field -> Fields
nAwayOn fields t n field --  - EXACTLY n away
  | t == (-1) = filter (\f -> fieldType f == 0) $ nAway fields (getAdjacency fields) n field -- only consider water fields
  | t == 0 = nAway fields (getWaterAdjacency fields) n field -- consider water + "moving along the shore"
  | t == 1 = filter (\f -> fieldType f /= 0) $ nAway fields (getAdjacency fields) n field -- only consider land
  | otherwise = nAway fields (getAdjacency fields) n field-- consider everything 

-- Is target field within n fields, on t-type?
targetInRange :: Fields -> Int -> Int -> (Int, Int) -> Bool
targetInRange fields t n (fromID, toID)-- 
  | fromID == toID = True
  | n == 1 = toID `elem` (map fieldID $ nAwayOn fields t 1 (fields !! fromID))
  | otherwise = toID `elem` (map fieldID $ nAwayOn fields t n (fields !! fromID))
                || targetInRange fields t (n-1) (fromID, toID)-- 

tankLegal :: Fields -> Order -> Bool -- Assumes order calls on tank unit, since it should only be called in such a case
tankLegal fields (Order f t (conq, as) _)-- 
 | t == 0 = targetInRange fields 1 1 (f, head as) || f == head as -- can move 1 on land, or conquer in place
 | t == 2 = targetInRange fields 2 1 (f, last as) -- can support 1 away anywhere (should only be offered if standing on unoccupied base)
 | otherwise = False-- 

shipLegal :: Fields -> Order -> Bool
shipLegal fields (Order f t (conq, as) _)-- 
 | t == 0 = targetInRange fields 0 1 (f, head as) || f == head as -- can move 1 on water, or conquer in place
 | t == 2 = targetInRange fields 0 1 (f, last as) -- can support 1 away on water
 | otherwise = False-- 

planeLegal :: Fields -> Order -> Bool
planeLegal fields (Order f t (conq, as) _)-- 
 | t == 0 = (targetInRange fields 2 2 (f, last as) -- should work with moving 1&2
            && targetInRange fields 2 1 (f, head as) -- moves through field which neighbors starting field
            && targetInRange fields 2 1 (head as, last as)) -- movest through field which neighbors target field
            || f == head as -- can move 2 anywhere, or conq
 | t == 2 = targetInRange fields 2 1 (f, last as) -- can support 1 away anywhere
 | otherwise = False-- 

supertankLegal :: Fields -> Order -> Bool
supertankLegal fields (Order f t (conq, as) _) -- Identical to tankLegal (for now)
 | t == 0 = targetInRange fields 1 1 (f, head as) || f == head as -- can move 1 on land, or conq
 | t == 2 = targetInRange fields 2 1 (f, last as) -- can support 1 away anywhere
 | otherwise = False-- 

submarineLegal :: Fields -> Order -> Bool
submarineLegal fields (Order f t (conq, as) _)-- 
 | t == 0 = (if length as == 1 then targetInRange fields 0 1 (f, head as) -- if moving 1, any water
              else -- if moving 2
                targetInRange fields (-1) 1 (f, head as) -- must move through sea...
                && targetInRange fields 0 1 (head as, last as) -- ...which borders the target
                && targetInRange fields 0 2 (f, last as) ) -- may arrive at any water
            || f == head as -- can move 2 on water, only last field can be land, or conq
 | t == 2 = targetInRange fields 2 1 (f, head as) -- can support 1 away anywhere
 | otherwise = False-- 

artilleryLegal :: Fields -> Order -> Bool
artilleryLegal fields (Order f t (conq, as) _)-- 
 | t == 0 = targetInRange fields 1 1 (f, head as) || f == head as -- can move 1 on land, or conq
 | t == 2 = targetInRange fields 2 2 (f, last as) -- can support 2 away anywhere
 | t == 4 = targetInRange fields 2 2 (f, head as) -- can bombard 2 away anywhere
 | otherwise = False

freeHomeBases :: Team -> Units -> [Int]
freeHomeBases team units = map fst . filter -- 
  (\(f, b) -> b && not (f `elem` map unitField units)) $ baseIDs team --

howManyNewUnits :: Int -> Team -> Units -> (Int, Int)
howManyNewUnits step team units -- How many new units can a team create?
  | step `mod` 2 == 0 = (0,0) -- If even turn, then none
  | otherwise = (dnormal, dsuper) -- If odd, then this difference
    where
      us = nUnits units team
      cap = unitCapacity team
      dnormal' = fst cap - fst us
      dsuper' = snd cap - snd us
      nFreeHomeBases = length $ freeHomeBases team units -- number of home bases without units on them
      dnormal = min dnormal' nFreeHomeBases
      dsuper = min dsuper' nFreeHomeBases
      -- 


isOrderLegal :: Fields -> Units -> Order -> Bool
isOrderLegal fields units order = foldr (uncurry if') False [--  DOES NOT DETECT non-attack/defense being supported
  (orderType order == 3, True), -- If unit wants to transform, it will be considered legal, to be checked at the end
  (orderType order == 1, True), -- Defending is always legal (have to make sure all orderAffects are properly formatted)
  (t == 0, tankLegal fields order), -- If unit is a tank
  (t == 1, shipLegal fields order), -- If unit is a ship
  (t == 2, planeLegal fields order), -- If unit is a plane
  (t == 3, supertankLegal fields order), -- ...
  (t == 4, submarineLegal fields order),
  (t == 5, artilleryLegal fields order)
  ]
  where
    unit = head . filter (\u -> unitField u == orderField order) $ units -- Orders should not reference nonexistent units
    t = unitType unit -- 

getTargetableFields :: Fields -> Int -> Unit -> Fields
getTargetableFields fields action unit = filter isTargetable fields
  where
    uf = fields !! unitField unit
    ut = unitType unit
    first = nAwayOn fields 2 1 uf
    second = nAwayOn fields 2 2 uf
    isOneRange = (ut `elem` [0,1,3,5] && action == 0) || (ut `elem` [0..4] && action == 2)
    inrange = if isOneRange
              then first
              else second 
    isTargetable field = 
      field `elem` inrange
        {-      && (not . null) 
          [()
          | frst <- first
          , let as' = if field `elem` first
                      then [field]
                      else [frst,field]
          , let as = map fieldID as'
          , isOrderLegal fields [unit] $ Order (unitField unit) action (True,as) Unresolved
          ]
          -}


applyDeltaOrders :: Teams -> Fields -> Units -> Orders -> (Units, Orders)
applyDeltaOrders teams fields units orders -- Creates / destroys units (new units, dead units), dead units can be killed by regular applyOrders as well
  | dorders == [] = (units, orders) -- if no new units, nothing to do
  | otherwise = (newUnits, orders `without` dorders)
    where
      dorder o = orderType o `elem` [-1,5] && (fst . orderAffects $ o) -- Checks if order changes the number of units (see data Order in Hadijatek.hs)
      news = filter (\o -> orderType o == 5) orders -- New unit orders
      deads = filter (\o -> orderType o == -1 && (fst . orderAffects $ o)) orders -- Kill unit orders
      dorders = news ++ deads -- delta orders
      newUnits = 
        units `without` (filter (\u -> unitField u `elem` map orderField deads) units) ++ -- old units without dead units
        [Unit (orderField new)
              (teamID . fromJust . teamOfOrder teams [] $ new) -- Can pass [] for units, because new ones will not be in there anyways
              (head . snd . orderAffects $ new)
          | new <- news] -- New units

applyOrders :: Teams -> Fields -> Units -> Orders -> (Teams, Units)
applyOrders teams fields units [] = (teams, units) -- Assumes orders are legal, and ARE SUCCESSFUL; outputs updated teams and updated units
applyOrders teams fields units (order:orders) = applyOrders newTeams fields newUnits orders
  where
    field = fields !! (last . snd . orderAffects $ order) -- Field where unit ended up
    newTeams = -- 
      [team
        | i <- [0..(length teams -1)],
          let team' = teams !! i,
          let oldBaseIDs = baseIDs team',
          let team = -- 
                if Just team' == teamOfOrder teams units order -- If team issued order
                then
                  Team (teamID team') (teamName team') (teamColor team') -- Same unchanged attributes
                    [base
                      | let (n, newBase) =
                              if fieldType field == 2 -- If the field is a base
                                  && (fst . orderAffects $ order) -- and the unit was ordered to conquer
                                  && not (fieldID field `elem` map fst oldBaseIDs) -- and the team doesn't own the base
                                then if fieldColor field == teamColor team' -- If the team takes back a home base
                                     then (length oldBaseIDs, [(fieldID field, True)]) -- isHomeBase
                                     else (length oldBaseIDs, [(fieldID field, False)]) -- regular base
                                else (length oldBaseIDs -1, []),
                        j <- [0..n],
                        let base = (oldBaseIDs ++ newBase) !! j]
                else 
                  Team (teamID team') (teamName team') (teamColor team') -- Same unchanged attributes
                    [base
                      | base <- oldBaseIDs,
                        fst base /= fieldID field] ] -- Same as before, except the base which was taken}}}
    newUnits = -- 
      [unit
        | i <- [0..(length units -1)],
          let unit' = units !! i,
          let utype =
                if orderType order == 3 -- If unit is to transform
                then (head . snd . orderAffects $ order) -- Get its type
                else unitType unit', -- Otherwise stays the same
          not $ orderType order == (-1) && (fst . orderAffects $ order), -- Unit isn't dead (unit can't retreat logic is done in resolveTurn.hs, and should be passed to this as (-2), (-1's not being applied until post-conflict))
          let unit =
                if unitField unit' == orderField order -- If the unit is being commanded
                then Unit (fieldID field) (unitTeam unit') (utype) -- Move it to the relevant field (does nothing if unit is stationary, assuming mapSucceededOrder has been applied)
                else unit' ]-- }}}

mapSucceededOrder :: Order -> Order
mapSucceededOrder (Order f t (b,as) status) -- Changes successful orders to ones that represent the final state of the board 
  | t `elem` [2,4] = Order f 1 (False, [f]) Unresolved -- Successful supports/bombards, switch to defending - no need to check for units passing through, those should all have succeeded / failed already. Should recheck with defence though, to see if it needs to retreat
  | t `elem` [0,1,3] = order -- Successful attack, defense, or transformation
  | otherwise = order -- For retreats
  where
    order = Order f t (b,as) status -- 

mapFailedOrder :: Order -> Order
mapFailedOrder (Order f t (b,as) status) -- Creates new order, which is more likely to succeed. 
  | t `elem` [2..4] = Order f 1 (True, [f]) Unresolved -- If a static unit fails, it switches to defending - letting all teams through
  | t == 1 = Order f (-1) (False, [f]) Unresolved -- If a defense fails, the unit must retreat
  | t == 0 && length as == 2 = Order f 0 (b,[last . init $ as]) Unresolved -- If attacking 2 away, change to 1
  | t == 0 && length as == 1 = Order f 1 (b,[f]) Unresolved -- If attacking 1 away, change to defense (technically the same as t `elem` [2..4])
  | otherwise = order -- For debugging purposes
  where
    order = Order f t (b,as) status -- 

possibleRetreats :: Teams -> Fields -> Units -> Orders -> Order -> Orders
possibleRetreats teams fields units originalOrders (Order f t (b,as) status) = -- 
  filter (isOrderLegal fields appliedUnits) -- filter for legal orders
    $ [Order f 0 (False,[dest]) Unresolved -- orders to move to all neighboring fields
       | dest <- map fieldID $ nAwayOn fields 2 1 (fields !! f), -- list of neighboring fields
         (null . filter (\u -> unitField u == dest) $ appliedUnits), -- which don't contain units
         not (dest `elem` -- dest. field is not a field from where a unit attacked
              (map (\o -> last (orderField o : (init . snd . orderAffects $ o))) . -- last place unit was before attacking - subject to change to simply orderField (imo bad idea)
                filter (\o -> orderType o == 0 && (last . snd . orderAffects $ o) == f) $ originalOrders)) ] 
  where
    (appliedTeams, appliedUnits) = applyOrders teams fields units originalOrders -- 

checkForDeath :: Teams -> Fields -> Units -> Orders -> Order -> Order
checkForDeath teams fields units originalOrders (Order f t (b,as) status) -- if order causes unit to die, return death order
  | isRetreatImpossible || isPlaneDeadOverWater = Order f (-1) (True,[f]) Unresolved
  | otherwise = order
  where
    order = Order f t (b,as) status
    -- (appliedTeams, appliedUnits) = applyOrders teams fields units originalOrders
    isPlaneDeadOverWater = 
      (unitType . head . filter (\u -> f == unitField u) $ units) == 2 -- unit is a plane
      && (fieldType $ fields !! (last as)) == 0 -- plane ends over water
      -- && (fieldType $ fields !! f) == 0 -- plane starts over water - uncomment if more water fields, else it dilutes the strength of soubmarines
    isRetreatImpossible = 
      t == (-1)
      && (null $ possibleRetreats teams fields units originalOrders order) -- 

sortOrdersByField :: Orders -> Orders
sortOrdersByField = sortBy (\(Order f1 _ (_, _) _) (Order f2 _ (_, _) _) -> compare f1 f2)

sortOrdersByTarget :: Orders -> Orders
sortOrdersByTarget = sortBy (\(Order _ _ (_, a1) _) (Order _ _ (_, a2) _) -> compare (last a1) (last a2))

detectConflicts :: Teams -> Fields -> Units -> Orders -> [Orders] -- Returns conflicting orders, in list of conflicts
detectConflicts teams fields units orders = -- Assumes new units have been applied (applyDeltaOrders)
  uniqueNonSubsetElems .  -- Unique conflicts (otherwise they'd show up at least 2x, and each support would show up separately)
  filter (\cs -> length cs > 1) . -- more than just the order
  zipWith (:) orders . -- add order itself to conflict
  map conflictingWith $ orders
  where
    target = last . snd . orderAffects -- field which unit is affecting
    defendingStraits = filter -- returns defended straits
      (`elem` (map fieldID $ filter isFieldStrait fields))
      (map orderField . filter (\o -> orderType o == 1) $ orders) 
    passthroughs' [] = []--  fields of straits that are passed through -- input f:as of order
    passthroughs' [a] = []
    passthroughs' (f:as) = 
      passthroughs' as ++ -- passthroughs of sub-movements (eg. plane: a->b->c, then fields a->b and b->c must be checked)
      (map fieldID . -- ids of fields
      filter --which
        (\field ->
          and . map
            (\f' ->
              fieldsAreNeighbors field (fields !! f') -- neighbors order's fields
              && fieldType (fields !! f') == 0 -- order's fields are sea
              && isFieldStrait field) -- field is a strait
          $ [f,head as]) 
      $ fields)-- 
    passthroughs o = passthroughs' (orderField o : (snd . orderAffects $ o)) -- passthroughs of order
    flyovers o = -- fields of flyovers - includes "swimunders"
      if orderType o == 0 -- If moving
        then (init . snd . orderAffects $ o) -- Then take all fields moved *through*
        else [] --
    transits o = uniques (passthroughs o ++ flyovers o) -- all fields transited through
    tidOfOrder o = teamID . fromJust . teamOfOrder teams units $ o
    -- Have to detect attack against support against itself -- tbd in resolveTurn by failing the attack first
    supportedTeam o = -- 
      if orderType o == 2
      then Left (head . snd . orderAffects $ o)  -- team of attack/defense a supporting unit supports
      else if orderType o `elem` [0,1]
        then Left (teamID . fromJust . teamOfOrder teams units $ o) -- team of attacker/defender
        else Right o -- or just the order
    isConflicting o1 o2 = -- does o1 conflict with o2 -- TODO sort according to type of conflict
      (target o1 == target o2 -- same targets
        && supportedTeam o1 /= supportedTeam o2) -- only false if both orders are in support of the same team
      || (or . map-- 
          (\(os, oa) -> orderType os `elem` [2,3,4] -- if os is stationary
                        && target oa == orderField os) -- and oa targets os
          $ [(o2,o1),(o1,o2)]) -- combinations of o1 and o2
      || (or . map-- 
          (\(om, od) -> 
            orderType om == 0 -- o"movement" must be moving
            && (
               (orderType od == 1 -- If od is defending
                && orderField od `elem` (flyovers om ++ passthroughs om) -- od is defending where om is flying over / passing through
                && not (tidOfOrder om `elem` (init . snd . orderAffects $ od)
                        || (fst . orderAffects $ od)) ) -- od does not allow team of om through, and it's not a failed static order (indicated by the True)
               || (orderType od == 2 -- if supporting
                   && orderField od `elem` flyovers om -- om *flies over* od (subject to change)
                   && not (tidOfOrder om `elem` (init . snd . orderAffects $ od))) -- od is not supporting th team of om
               || (orderType od == 4 -- if od is bombarding
                   && target od `elem` flyovers om) -- om *flies over* bombardment
               ))
          $ [(o2,o1),(o1,o2)]) -- order 1 tries to "pass through" field defended by o2 (strait or flying over / swimming under) (or vice versa)
    conflictingWith o = filter (isConflicting o) orders-- list of all orders which conflict with o. o does not conflict with itself, or supporting units

