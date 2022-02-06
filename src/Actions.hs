module Actions where

import World
import Parsing



{-
data Object' = Coffee | Door | Mug | Coffeepot | FullMug
   deriving Show 
-}


data Command = Go Direction | Get Object 
               | Pour Object
               | Examine Object | Drink Object
               | Open | Wear Object | Wash Object
   deriving Show 

{-
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
-}


validate :: String -> String -> Maybe Command
validate "go" "north" = Just (Go North)
validate "go" "south" = Just (Go South)
validate "go" "east" = Just (Go East)
validate "go" "west" = Just (Go West)
validate "go" "out" = Just (Go Out)
validate "go" "in" = Just (Go In)
validate "get" "mug" = Just (Get mug)
validate "get" "mask" = Just (Get mask)
validate "get" "pot" = Just (Get coffeepot)
validate "get" "soap" = Just (Get soap)
--validate "get" "coffee" = Just (Get coffee)
--validate "drop" "mug" = Just (Drop mug)
--validate "drop" "coffee" = Just (Drop Coffee)
validate "pour" "coffee" = Just (Pour coffeepot)
validate "examine" "mug" = Just (Examine mug)
validate "examine" "coffee" = Just (Examine fullmug)
validate "examine" "coffeepot" = Just (Examine coffeepot)
validate "examine" "soap" = Just (Examine soap)
validate "examine" "mask" = Just (Examine mask)
validate "drink" "coffee" = Just (Drink fullmug)
validate "open" "door" = Just (Open)
validate "wear" "mask" = Just (Wear mask)
validate "wash" "hands" = Just (Wash soap)
validate _ _ = Nothing

-- commandOther :: String -> Maybe Command 
-- commandOther "quit" = Just quit 
-- commandOther "inv" = Just inv
-- commandOther _ = Nothing

performAction :: GameData -> Command -> (GameData, String)
performAction gd (Go d)            = go d gd
performAction gd (Get obj)         = get obj gd
performAction gd (Pour obj)        = pour obj gd
performAction gd (Examine obj)     = examine obj gd
performAction gd (Drink obj)       = drink obj gd
performAction gd (Open)            = open mug gd -- mug isnt actually used here can change later
performAction gd (Wear obj)        = wear obj gd
performAction gd (Wash obj)        = wash obj gd

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

instructions :: String -> Maybe Instruction
instructions "quit"      = Just quit
instructions "inventory" = Just inv
instructions _           = Nothing


{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: Direction -> Room -> Maybe String
   {- check each of the exits in the room and if one matches the direction given return that exit
      else returun Nothing
   -}
move dir rm = if (res == []) then Nothing else Just (room (head res))
      where res = filter (\x -> dir == exit_dir x) (exits rm)
--move dir rm = let exits = (exits rm)
  --                in if()

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
addInv gd obj 
   | objectHere obj (getRoomData gd) = gd {inventory = (inventory gd) ++ [obj]}
   | otherwise = gd
            
{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> Object -> GameData
removeInv gd obj 
   | carrying gd obj = gd {inventory = filter (\x -> x /= obj) (inventory gd)}
   | otherwise = gd

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

go :: Direction -> GameData -> (GameData, String)
go dir state = case move dir (getRoomData state) of
   Just ans -> (state {location_id=ans},"OK")
   Nothing -> (state,"You are unable to move this way.")

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



get :: Action --have not used objectData??!!
{-
get obj state | objectHere obj (getRoomData state) = (updateRoom state (location_id state) (removeObject obj (getRoomData (addInv state obj))),"OK") 
              | otherwise                          = (state, "Item not in room")
-}

get obj state | objectHere obj (getRoomData state) = let newState = addInv state obj
                                                         in (updateRoom newState (location_id newState) (removeObject obj (getRoomData newState)),"OK")

              | otherwise                          = (state, "Item not in room")             
-- {- Remove an item from the player's inventory, and put it in the current room.
--    Similar to 'get' but in reverse - find the object in the inventory, create
--    a new room with the object in, update the game world with the new room.
-- -}

-- -- obtainObj :: GameData -> String -> Object 
-- -- obtainObj gd obj = head (filter(\x -> obj_name x == obj) (inventory gd))

put :: Action
put obj state | carrying state obj = let newState = removeInv state obj
                                                         in (updateRoom newState (location_id newState) (addObject obj (getRoomData newState)),"OK")
              | otherwise          = (state, "Item not in inventory")


{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state | carrying state obj                 = (state, obj_desc obj)
                  | objectHere obj (getRoomData state) = (state, obj_desc obj)
                  | otherwise                          = (state, "Cannot examine object")


{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}


pour :: Action
{-an idea?? create makeFullMug function-}
pour obj state | carrying state mug && carrying state coffeepot = (state {inventory= filter (/= mug) (inventory state) ++ [fullmug]},"OK")
               | otherwise                          = (state,"Cannot pour object")
{-}
pour :: Action
{-an idea?? create makeFullMug function-}
pour obj state | carrying state mug && carrying state coffeepot = (state {inventory= filter (/= mug) (inventory state) ++ [fullmug]},"OK")
               | otherwise                          = (state,"Cannot pour object")
-}
-- pour Coffee state = if carrying mug then 
--                         if carrying coffeepot then 
--                            do state <- addInv state fullmug 
--                               state <- removeInv state mug 
--                               (state, "OK")
--                           else (state, "You do not have the required items.")
--                           else (state, "You do not have the required items.")
--                   -- not sure if this will work, and the indentation may be off.
-- pour _ state = (state, "Cannot pour this object.")

               

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state | carrying state fullmug = (state {inventory= filter (/= fullmug) (inventory state) ++ [mug], caffeinated=True}, "OK")
                | otherwise              = (state,"You are not carrying a full coffe mug")

-- drink Coffee state = if carrying fullmug  then 
--                         do state <- addInv state mug 
--                            state <- removeInv state fullmug
--                            state { caffeinated = True }  
--                            (state, "OK")
--                            else (state, "You do not have the required items.")   
--                   -- not sure if this will work, and the indentation may be off.
-- drink _ state = (state, "Cannot drink this object.")
               
{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}
wear :: Action
wear obj state | carrying state mask = (state {inventory= filter (/= mask) (inventory state), maskOn=True}, "OK, you are now wearing your stylish facemask")
               | otherwise              = (state,"You need to be carrying a mask to wear it.")

wash :: Action
wash obj state | carrying state soap = (state {handsWashed = True}, "OK, your hands are now clean.")
               | otherwise              = (state,"You need to wash your hands with soap.")


open :: Action
open _ state | caffeinated state && getRoomData state == hall && maskOn state && handsWashed state = (updateRoom state "hall" (Room openedhall openedexits []),"OK")
             | otherwise         = (state, "You need to be caffeinated and fully covid safe before you go outside.")

         

{- Don't update the game state, just list what the player is carrying -}

inv :: Instruction
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Instruction
quit state = (state { finished = True }, "Bye bye")
