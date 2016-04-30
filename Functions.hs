module Functions where
import Data.Text (pack, toLower)	
import Types
--version0.7


invalid :: GameState -> GameState
invalid (GameState world player message turns) = (GameState world player "Not a command. Please try again" turns)


quit :: GameState -> GameState
quit (GameState world player message turns) = (GameState world player "quit" turns)

help :: GameState -> GameState
help (GameState world player message turns) = 
	(GameState world player
		(
			"Commands:\n--------------------------------------------------------------------------------\n"++
			"(Q)uit: Exit the game\n"++
			"e(X)amine: Gives a description of your current location and a name of a nearby item\n"++
			"(C)heck Status: Shows you your player information and how many moves are left\n"++
			"(I)nventory: Shows you your current items\n"++
			"(T)ake [ITEM NAME]: Pick up a desired item\n"++
			"(D)rop [ITEM NAME]: Drop a desired item\n"++
			"(G)ame: Displays information about the current game state (for developers only)\n"++
			"Move: Type the first letter of the cardinal direction you want to go (N,E,S,W)\n"++
			"(H)elp: Display this list of commands\n"++
			"--------------------------------------------------------------------------------"
		)
		turns
	)


examine :: GameState -> GameState
examine state@(GameState world player message turns) = 
	if (isEmpty loc) then
		(GameState (search world) player 
			(directions (getAdjacentLocs state)) turns)
	else
		(GameState (search world) player 
			((directions (getAdjacentLocs state))++"\tYou notice these items nearby: "++(unwords (map name (contents loc))) ) turns)
	where
		loc = (worldLocs world)!!(playerLoc player)
		search w@(World ls cs) = (World (locs (locID loc)) cs)
		locs n = ((take n (worldLocs world))++[(Location (locID loc) (name loc) (desc loc) (contents loc) (locEnemy loc) True)]++(drop (n+1) (worldLocs world)))
		directions [] = []
		directions ((l,d):lds) = "To your "++(show d)++" is a "++(name l)++".\n"++(directions lds)


look :: GameState -> GameState
look state@(GameState world player message turns) = 
	(GameState world player ("You look around and see a "++(desc loc)) turns) where
		loc = (worldLocs world)!!(playerLoc player)


inventory :: GameState -> GameState
inventory (GameState world player@(Player _ _ _ inv _) message turns) =
	if (isEmpty player) then 
		(GameState world player ("You aren't holding anything.") turns)
	else
		(GameState world player ("Your items:\n----------------------------\n"++(unwords (map name (contents player)))) turns)


pickUp :: GameState -> String -> GameState
pickUp (GameState world player@(Player _ _ _ inv _) message turns) req =
	if ((searched loc) == False) then
		GameState world player "You have not searched your surroundings!" turns
	else if (isEmpty loc) then
		GameState world player "There is no item to pick up!" turns
	else if (reqL `elem` (map (toLower.pack.name) (contents loc))) == False then  
		GameState world player "That is not an item you can pick up!" turns
	else
		(
		GameState 
			(World (locs (playerLoc player)) (worldCons world))
			(acquire player item) 
			("You pick up the "++req++".")
			(turns+1)
		)
	where
		reqL = toLower (pack req) 
		loc = (worldLocs world)!!(playerLoc player)
		item = head (filter ((==reqL).(toLower.pack.name)) (contents loc))
		locs n = ((take n (worldLocs world))++[(release loc item)]++(drop (n+1) (worldLocs world)))


ditch :: GameState -> String -> GameState
ditch (GameState world player@(Player _ _ _ inv _) message turns) req = 
	if (isEmpty player) then 
		GameState world player "You have nothing to drop!" turns
	else if (reqL `elem` (map (toLower.pack.name) (contents player))) == False then
		GameState world player "You don't have that to drop!" turns
	else
	(
		GameState
			(World (locs (playerLoc player)) (worldCons world))
			(release player item)
			("You drop the "++req++".")
			(turns+1)
	)
	where
		reqL = toLower (pack req)
		loc = (worldLocs world)!!(playerLoc player)
		item = head (filter ((==reqL).(toLower.pack.name)) (contents player))
		locs n = ((take n (worldLocs world))++[(acquire loc item)]++(drop (n+1) (worldLocs world)))

getItems :: World -> [[Item]]
getItems world = map (getItem) (worldLocs world) where
	getItem loc = locItem loc

getItemsByID :: World -> ID -> [Item]
getItemsByID world id = (getItems world)!!id


move :: GameState -> Dir -> GameState
move state@(GameState world player message turns) req = 
	if req `elem` (map (getDir) (getAdjacentLocs state)) then
		(GameState 
			world 
			(Player 
				(name player) 
				(playerGender player) 
				(locID loc) 
				(stuff player) 
				(stillAlive player)
			) 
			("You head for the "++(name loc)) 
			(turns+1)
		)
	else
		(GameState world player "You cannot go there!" turns)
	where loc = (getLoc (head (filter ((==req).getDir) (getAdjacentLocs state))))

getAdjacentLocs :: GameState -> [(Location, Dir)]
getAdjacentLocs state@(GameState world player message turns) = gALHelper locList curLocCons where
	locList = (worldLocs world)
	curLocCons = (worldCons world)!!(playerLoc player)
	gALHelper _ [] = []
	gALHelper [] _ = []
	gALHelper locList conList =
		if (head conList) == 1 then ((head locList), North):(gALHelper (tail locList) (tail conList))
		else if (head conList) == 2 then ((head locList), East):(gALHelper (tail locList) (tail conList))
		else if (head conList) == 3 then ((head locList), South):(gALHelper (tail locList) (tail conList))
		else if (head conList) == 4 then ((head locList), West):(gALHelper (tail locList) (tail conList))
		else (gALHelper (tail locList) (tail conList))

getDir :: (Location, Dir) -> Dir
getDir (l, d) = d

getLoc :: (Location, Dir) -> Location
getLoc (l, d) = l


checkStatus :: GameState -> GameState
checkStatus state@(GameState world player message turns) = 
	(
		GameState world player
		("Player Status:\n-------------------\n"++(desc player)++"\nMoves Remaining: "++(show (20-turns)))
		turns
	)
