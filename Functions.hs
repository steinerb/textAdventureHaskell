module Functions where
import Data.Char
import Types
--version0.7


invalid :: GameState -> GameState
invalid (GameState world player message turns) = (GameState world player "Not a command. Please try again" turns)


quit :: GameState -> GameState
quit (GameState world player message turns) = (GameState world player "quit" turns)


help :: IO ()
help = do
	putStrLn ("Commands:\n--------------------------------------------------------------------------------")
	putStrLn ("(Q)uit: Exit the game")
	putStrLn ("e(X)amine: Gives a description of your current location and a name of a nearby item")
	putStrLn ("(C)heck Status: Shows you your player information and how many moves are left")
	putStrLn ("(I)nventory: Shows you your current items")
	putStrLn ("(T)ake [ITEM NAME]: Pick up a desired item")
	putStrLn ("(D)rop [ITEM NAME]: Drop a desired item")
	putStrLn ("(G)ame: Displays information about the current game state (for developers only)")
	putStrLn ("Move: Type the first letter of the cardinal direction you want to go (N,E,S,W)")
	putStrLn ("(H)elp: Display this list of commands")
	putStrLn ("--------------------------------------------------------------------------------")
	

examine :: GameState -> GameState
examine (GameState world player message turns) = 
	if (isEmpty loc)
		then (GameState (search world) player 
			("You look around and see "++(desc (loc)) ) turns)
	else
		(GameState (search world) player 
			("You look around and see "++(desc (loc))++"\n\t\tYou notice these items nearby: "++(unwords (map name (contents loc))) ) turns)
	where
		loc = (worldLocs world)!!(playerLoc player)
		search w@(World ls cs) = (World (locs (locID loc)) cs)
		locs n = ((take n (worldLocs world))++[(Location (locID loc) (name loc) (desc loc) (contents loc) (locEnemy loc) True)]++(drop (n+1) (worldLocs world)))


inventory :: GameState -> GameState
inventory (GameState world player@(Player _ _ _ inv _) message turns) =
	if (isEmpty player) then 
		(GameState world player ("You aren't holding anything.") turns)
	else
		(GameState world player ("Your items:\n----------------------------\n"++(unwords (map name (contents player)))) turns)

--editing to make case sensitive (lines 56 & 68)
pickUp :: GameState -> String -> GameState
pickUp (GameState world player@(Player _ _ _ inv _) message turns) req =
	if ((searched loc) == False) then
		GameState world player "You have not searched your surroundings!" turns
	else if (isEmpty loc) then
		GameState world player "There is no item to pick up!" turns
	else if (req `elem` (map (name) (contents loc))) == False then  
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
		loc = (worldLocs world)!!(playerLoc player)
		item = head (filter ((==req).(name)) (contents loc))
		locs n = ((take n (worldLocs world))++[(release loc item)]++(drop (n+1) (worldLocs world)))

--editing to make case sensitive (lines 76 & 88)
ditch :: GameState -> String -> GameState
ditch (GameState world player@(Player _ _ _ inv _) message turns) req = 
	if (isEmpty player) then 
		GameState world player "You have nothing to drop!" turns
	else if (req `elem` (map (name) (contents player))) == False then
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
		loc = (worldLocs world)!!(playerLoc player)
		item = head (filter ((==req).(name)) (contents player))
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
