module Functions where
import Data.Text (pack, toLower)	
import Types
--version0.7


invalid :: GameState -> GameState
invalid (GameState world player message turns) = (GameState world player "Not a command. Please try again" turns)

answer :: GameState -> Int -> GameState
answer state@(GameState world player message turns) req = 
	--not [in] Trapped
	if not ((playerLoc player) == 7) then
		invalid state
	--wrong answer
	else if (req == 1) then
		(GameState
			world
			(Player 
				(name player)
				(playerGender player)
				(playerLoc player)
				(contents player)
				False
			)
			("\"Inspecta Deck!\", you shout.\n The Guard shakes his head before stabbing you with his spear.")
			(turns+1) 
		)
	--correct answer
	else if (req == 2) then
		GameState
			(World
				(
					[(worldLocs world)!!0]++
					[(worldLocs world)!!1]++
					[(worldLocs world)!!2]++
					[(worldLocs world)!!3]++
					[(worldLocs world)!!4]++
					[(Location
						(locID gate)
						(name gate)
						(desc gate)
						(contents gate)
						(Just (Enemy "GateKeeper" False))
						(searched gate)
					)]++
					[(worldLocs world)!!6]++
					[(Location
						(locID trapped)
						(name trapped)
						(desc trapped)
						(contents trapped)
						(Just (Enemy "GateKeeper" False))
						(searched trapped)
					)]
				)
				(worldCons world)
			)
			(Player
				(name player)
				(playerGender player)
				(5)
				(contents player)
				(stillAlive player)
			)
			("\"Ol' Dirty Bastard!\", you shout.\n The Guard smiles before bursting into flames.")
			(turns+1)
	--wrong answer
	else if (req == 3) then
		(GameState
			world
			(Player 
				(name player)
				(playerGender player)
				(playerLoc player)
				(contents player)
				False
			)
			("\"RZA!\", you shout.\n The Guard shakes his head before stabbing you with his spear.")
			(turns+1) 
		)
	--wrong answer
	else if (req == 4) then
		(GameState
			world
			(Player 
				(name player)
				(playerGender player)
				(playerLoc player)
				(contents player)
				False
			)
			("\"GZA!\", you shout.\n The Guard shakes his head before stabbing you with his spear.")
			(turns+1) 
		)
	--invalid answer
	else
		(GameState world player "Not an answer. Please try again" turns)
	where
		gate = (worldLocs world)!!5
		trapped = (worldLocs world)!!7
		

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
			"(U)se  [ITEM NAME]: Use a desired item\n\t"++
			"*CAUTION*: using a weapon in the same room as an enemy is an attempt at an attack!\n"++
			"(G)ame: Displays information about the current game state (for developers only)\n"++
			"Move: Type the first letter of the cardinal direction you want to go (N,E,S,W)\n"++
			"(H)elp: Display this list of commands\n"++
			"--------------------------------------------------------------------------------"
		)
		turns
	)

--will display the riddle/question when in Trapped
examine :: GameState -> GameState
examine state@(GameState world player message turns) =
	--examine when [in] Trapped 
	if (playerLoc player == 7) then
		(GameState (search world) player 
			(
				"Which member of the Wu Tang Clan is no longer with us?\n"++
				"------------------------------------------------------------\n"++
				"\t(1) Inspecta Deck\n"++
				"\t(2) Ol' Dirty Bastard\n"++
				"\t(3) RZA\n"++
				"\t(4) GZA"
			)
			turns
		)
	--examine an empty location
	else if (isEmpty loc) then
		(GameState (search world) player 
			(directions (getAdjacentLocs state)) turns)
	--regular examine
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
	(GameState world player ("You look around and see "++(desc loc)) turns) where
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
	else if reqL == (pack "fists") then
		GameState world player "You cannot drop these!" turns
	else if (reqL `elem` (map (toLower.pack.name) (contents player))) == False then
		GameState world player "You don't have that to drop!" turns
	else
	(
		GameState
			(World (locs (playerLoc player)) (worldCons world))
			(release player item)
			("You part ways with your "++req++".")
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


use :: GameState -> String -> GameState
use state@(GameState world player message turns) req = 
	--don't have an item
	if ((filter ((==reqL).(toLower.pack.name)) (contents player)) == []) then
		(GameState world player "You do not have that item to use!" turns)
	--use a Note
	else if (name item == "Note") then
		(GameState world player ("The note reads:\n\t"++(desc item)) turns)
	--use a Ticket at townstation
	else if ((playerLoc player == 2) && (name item == "Ticket")) then
		(move state North)
	--use any weapon that isn't the wuTangSword sword (a RustySword) on enemy at Street
	else if ((playerLoc player == 4) && ((name item == "RustySword") || (name item == "Fists"))) then
		(GameState
			world
			(Player 
				(name player)
				(playerGender player)
				(playerLoc player)
				(contents player)
				False
			)
			("You lunge towards the pimp, who then shoots you on sight.")
			(turns+1) 
		)
	--use any weapon that isn't the wuTangSword (a Rusty Sword) on enemy at gate
	else if ((playerLoc player == 5) && ((name item == "RustySword") || (name item == "Fists"))) then
		(GameState
			world
			(Player
				(name player)
				(playerGender player)
				7
				(contents player)
				(stillAlive player)
			)
			("You strike the Gate Keeper's armor with your "++(name item)++". He then teleports you to another dimension!")
			(turns+1)
		)
	--more conditions go here

	--can't use an item in a place
	else
		(GameState world player "You can't use that here!" turns)
	where
		reqL = toLower (pack req)
		item = head (filter ((==reqL).(toLower.pack.name)) (contents player))


move :: GameState -> Dir -> GameState
move state@(GameState world player message turns) req = 
	--no ticket at townstation
	if ((playerLoc player == 2) && (req == North) && not ("Ticket" `elem` (map name (contents player)))) then
		(GameState world player "what could be used to travel to the next station?" turns)
	--ticket at town station
	else if ((playerLoc player == 2) && (req == North)) then
		(ditch 
			(GameState 
				world
				(Player 
					(name player)
					(playerGender player)
					(locID loc)
					(contents player)
					(stillAlive player)
				)
				("You head for the "++(name loc))
				(turns)
			)
			"Ticket"
		)
	--regular movement
	else if req `elem` (map (getDir) (getAdjacentLocs state)) then
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
	--can't go there
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
