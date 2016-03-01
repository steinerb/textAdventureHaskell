module Functions where
import Types
import Data.Maybe (fromJust)
--version0.1

invalid :: GameState -> GameState
invalid (GameState world player message turns) = (GameState world player "Not a command. Please try again" turns)

quit :: GameState -> GameState
quit (GameState world player message turns) = (GameState world player "quit" turns)

look :: GameState -> GameState
look (GameState world player message turns) = 
	if (getItemByID world (playerLoc player)) == Nothing
		then (GameState world player 
			("You look around and see a "++(locDesc ((worldLocs world)!!(playerLoc player)))) turns)
	else
		(GameState world player 
			("You look around and see a "++(locDesc ((worldLocs world)!!(playerLoc player)))++"\n\tYou notice a "++(itemName $ fromJust (getItemByID world (playerLoc player)))++" nearby.") turns)

pickUp :: GameState -> GameState
pickUp (GameState world player message turns) = 
	if (getItemByID world (playerLoc player)) == Nothing
		then GameState world player "There is no item to pick up!" turns
	else
	(
		GameState 
			(World (locs (playerLoc player)) (worldCons world))
			(Player (playerName player) (playerLoc player) (getItemByID world (playerLoc player)) True) 
			("You pick up the item.")
			(turns+1)
	)
	where locs n = (take n (worldLocs world))++[Location n (locName loc) (locDesc loc) Nothing (locEnemy loc)]++(drop (n+1) (worldLocs world))
		where loc = (worldLocs world)!!(playerLoc player)

ditch :: GameState -> GameState
ditch (GameState world player message turns) = 
	if (inventory player) == Nothing
		then GameState world player "You have nothing to drop!" turns
	else
	(
		GameState
			(World (locs (playerLoc player)) (worldCons world))
			(Player (playerName player) (playerLoc player) Nothing True)
			("You drop the item you were holding.")
			(turns+1)
	)
	where locs n = (take n (worldLocs world))++[Location n (locName loc) (locDesc loc) (inventory player) (locEnemy loc)]++(drop (n+1) (worldLocs world))
		where loc = (worldLocs world)!!(playerLoc player)

getItems :: World -> [Maybe Item]
getItems world = map (getItem) (worldLocs world) where
	getItem loc = locItem loc

getItemByID :: World -> ID -> Maybe Item
getItemByID world id = (getItems world)!!id