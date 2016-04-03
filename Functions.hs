module Functions where
import Types
--version0.3


invalid :: GameState -> GameState
invalid (GameState world player message turns) = (GameState world player "Not a command. Please try again" turns)


quit :: GameState -> GameState
quit (GameState world player message turns) = (GameState world player "quit" turns)


help :: IO ()
help = do
	putStrLn ("Commands:\n--------------------------------------------------------------------------------")
	putStrLn ("(Q)uit: Exit the game")
	putStrLn ("(L)ook: Gives a description of your current location and a name of a nearby item")
	putStrLn ("(T)ake: Pick up the item in your current location")
	putStrLn ("(D)rop: Drop the item in your inventory to your current location")
	putStrLn ("(S)tatus: Displays information about the current game state (for developers only)")
	putStrLn (" Move: Type at least the first 3 letters of the location you wish to go to.")
	putStrLn ("(H)elp: Display this list of commands")
	putStrLn ("--------------------------------------------------------------------------------")
	

--fix the listing of Item names
look :: GameState -> GameState
look (GameState world player message turns) = 
	if (getItemByID world (playerLoc player)) == []
		then (GameState world player 
			("You look around and see "++(desc ((worldLocs world)!!(playerLoc player)))) turns)
	else
		(GameState world player 
			("You look around and see "++(desc ((worldLocs world)!!(playerLoc player)))++"\n\t\tYou notice these items nearby: "++ (unwords (map name (getItemByID world (playerLoc player)))) ) turns)


pickUp :: GameState -> GameState
pickUp (GameState world player@(Player _ _ _ inv _) message turns) = 
	if (getItemByID world (playerLoc player)) == []
		then GameState world player "There is no item to pick up!" turns
	else
	(
		GameState 
			(World (locs (playerLoc player)) (worldCons world))
			(Player (name player) (playerGender player) (playerLoc player) (inv++(getItemByID world (playerLoc player))) True) 
			("You pick up the item.")
			(turns+1)
	)
	where locs n = (take n (worldLocs world))++[Location n (name loc) (desc loc) (tail (locItem loc)) (locEnemy loc)]++(drop (n+1) (worldLocs world))
		where loc = (worldLocs world)!!(playerLoc player)

ditch :: GameState -> GameState
ditch (GameState world player@(Player _ _ _ inv _) message turns) = 
	if (inventory player) == []
		then GameState world player "You have nothing to drop!" turns
	else
	(
		GameState
			(World (locs (playerLoc player)) (worldCons world))
			(Player (name player) (playerGender player) (playerLoc player) (tail inv) True)
			("You drop the item you were holding.")
			(turns+1)
	)
	where locs n = (take n (worldLocs world))++[Location n (name loc) (desc loc) ((locItem loc)++[(head (inventory player))]) (locEnemy loc)]++(drop (n+1) (worldLocs world))
		where loc = (worldLocs world)!!(playerLoc player)

getItems :: World -> [[Item]]
getItems world = map (getItem) (worldLocs world) where
	getItem loc = locItem loc

getItemByID :: World -> ID -> [Item]
getItemByID world id = (getItems world)!!id


move :: GameState-> Dir -> GameState
move state@(GameState world player message turns) req = 
	if (toString req) `elem` (map (name) (getAdjacentLocs state)) then
		(GameState 
			world 
			(Player (name player) (playerGender player) (getID req) (inventory player) (stillAlive player)) 
			("You head for the "++(toString req)) 
			(turns+1)
		)
	else
		(GameState world player "You cannot go there!" turns)

getAdjacentLocs :: GameState -> [Location]
getAdjacentLocs state@(GameState world player message turns) = gALHelper locList curLocCons where
	locList = (worldLocs world)
	curLocCons = (worldCons world)!!(playerLoc player)
	gALHelper _ [] = []
	gALHelper [] _ = []
	gALHelper locList conList =
		if (head conList) == 1 then (head locList):(gALHelper (tail locList) (tail conList))
		else (gALHelper (tail locList) (tail conList))


