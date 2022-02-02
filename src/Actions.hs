module Actions where

import World
import Parsing



data Direction = North | South | East | West
   deriving Show 

data Object' = Coffee | Door | Mug
   deriving Show 

data Command' = Go Direction | Get Object' 
               | Drop Object' | Pour Object'
               | Examine Object' | Drink Object'
               | Open Object' 
   deriving Show 

type Action' = Object -> GameData -> (GameData,String)

parseDirection :: Parser Direction
parseDirection
     = do string "north"
          return North
   ||| do string "south"
          return South
   ||| do string "east"
          return East  
   ||| do string "west"
          return West  

validate :: String -> String -> Maybe Command'
validate "go" "north" = Just (Go North)
validate "go" "south" = Just (Go South)
validate "go" "east" = Just (Go East)
validate "go" "west" = Just (Go West)
validate "get" "mug" = Just (Get Mug)
validate "get" "coffee" = Just (Get Coffee)
validate "drop" "mug" = Just (Drop Mug)
validate "drop" "coffee" = Just (Drop Coffee)
validate "pour" "coffee" = Just (Pour Coffee)
validate "examine" "mug" = Just (Examine Mug)
validate "examine" "coffee" = Just (Examine Coffee)
validate "drink" "coffee" = Just (Drink Coffee)
validate "open" "door" = Just (Open Door)
validate _ _ = Nothing

-- commandOther :: String -> Maybe Command 
-- commandOther "quit" = Just quit 
-- commandOther "inv" = Just inv
-- commandOther _ = Nothing

-- actions :: String -> Maybe Action
-- actions "go"      = Just go 
-- actions "get"     = Just get
-- actions "drop"    = Just put
-- actions "pour"    = Just pour
-- actions "examine" = Just examine
-- actions "drink"   = Just drink
-- actions "open"    = Just open
-- actions _         = Nothing

-- parseObject :: String -> Maybe Object
-- parseObject "mug"     = Just mug
-- parseObject "coffee"  = Just coffeepot
-- parseObject _         = Nothing 


-- directions :: String -> Maybe Direction
-- directions "north"   = Just North
-- directions "south"   = Just South
-- directions "east"    = Just East
-- directions "west"    = Just West
-- directions _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing


{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
   {- check each of the exits in the room and if one matches the direction given return that exit
      else returun Nothing
   -}
move dir rm = if (res == []) then Nothing else Just $ exit_desc (head res) 
      where res = filter (\x -> dir == exit_dir x) (exits rm)

{- PARTIALLY COMPLETED!! (rather than taking a String, change it to take an Object)
Return True if the object appears in the room. -}

objectHere :: Object -> Room -> Bool
objectHere o rm = o `elem` (objects rm)
-- objectHere :: String -> Room -> Bool
-- objectHere o rm = o `elem` (map (\x -> obj_name x) (objects rm))


{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: Object -> Room -> Room
removeObject o rm = rm { objects = filter (/= o) (objects rm) }
{-the above was a trial, but it does not seem to be working-}
-- removeObject :: String -> Room -> Room 
-- removeObject o rm = rm {objects = filter (\x -> (obj_name x /= o)) (objects rm)}


{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm { objects = (objects rm) ++ [o]}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj o ds = head (filter (\x -> (obj_name x) == o) ds) 
{-head is safe because we assume that the object is in the list -}

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object 
objectData o rm = findObj o (objects rm)

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

findRoom :: GameData -> String -> Bool 
findRoom gd rmid = if filter (\(x,y) -> x == rmid) (world gd) == [] then False else True 


updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = if findRoom gd rmid then gd {world=[if (x==rmid) then (rmid,rmdata) else (x,y)| (x,y) <- world gd]} else gd {world=(world gd ++ [(rmid,rmdata)])}
               

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

-- addInv :: GameData -> String -> GameData
-- addInv gd obj = let currRoom = getRoomData gd 
--                     wantedObj = objectData obj currRoom
--                 in gd {inventory = (inventory gd) ++ [wantedObj]}
addInv :: GameData -> Object -> GameData 
addInv gd obj = gd {inventory = (inventory gd) ++ [obj]}
            
{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj = gd {inventory = filter (\x -> obj_name x == obj) (inventory gd)}

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> Object -> Bool
carrying gd obj = obj `elem` inventory gd
-- carrying :: GameData -> String -> Bool 
-- carrying gd obj = obj `elem` (map (\x -> obj_name x) (inventory gd))

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state = case move dir (getRoomData state) of
   Just ans -> (state {location_id=ans},"OK")
   Nothing -> (state,"Unable to move!")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}


get :: Action' --"obj" is an actual obj instead of a string

get obj state | objectHere obj (getRoomData state) = (latestState,"OK") where latestState = updateRoom (newState (location_id newState) (newRoom)) where newRoom = removeObj obj (getRoomData newState) where newState = addInv state obj
                  -- let newState = addInv state obj --use objectData?
                  --     newRoom = removeObj obj (getRoomData newState)
                  --     latestState = updateRoom (newState (location_id newState) (newRoom))
                  -- in (latestState,"OK")
              | otherwise                          = (state,"Item not in room")

-- get obj state = if objectHere obj rm then
--                   addInv (state, obj)
--                   (updateRoom state (location_id state) (removeObject obj rm), "OK")
--                 else (state, "That object is not in this room")
--                where rm = getRoomData
                  

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

-- obtainObj :: GameData -> String -> Object 
-- obtainObj gd obj = head (filter(\x -> obj_name x == obj) (inventory gd))

put :: Action'
put obj state = if carrying state obj then 
                  let newState = removeInv (state obj)
                      newRoom = addObj ((obtainObj newState obj) (getRoomData newState)) --finding actual object by going through the state's inv objects
                      latestState = updateRoom (newState (location_id newState) (newRoom))
                  in (newState,"OK")
               else 
                  (state,"Item not in inventory")

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action'
examine obj state | carrying state obj                 = (state, obj_desc obj)
                  | objectHere obj (getRoomData state) = (state, obj_desc (obj (getRoomData state)))
                  | otherwise                          = (state, "Cannot examine object")


{-YUE NING WILL DO UP TO HERE!! :) -}

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state = undefined

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state = undefined

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state = undefined

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")
