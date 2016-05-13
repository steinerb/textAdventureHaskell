import Types
import GameData
import Functions
--version0.7


main :: IO ()
main = textAdventure

textAdventure :: IO ()
textAdventure = do
	intro
	name <- getName
	gender <- getGender
	gameLoop 
		(GameState 
			(gameWorld gender) 
			(Player name gender 0 [(Item 900 "Fists" "Fists Desc" (-1))] True) 
			("You have "++(show maxMoves)++" moves until it's too late.\nYou wake up to an unusually quiet household.\nAfter looking around for quite some time, you realize your family is nowhere to be found.\nEverything in the house is exactly as they would have left it, except for a note on the table.") 
			0
		)
	--outro


intro :: IO ()
intro = do putStrLn "Welcome!!!"

getName :: IO String
getName = do
	putStrLn "Enter your name:"
	rawInput <- getLine
	nInput <- return (read rawInput :: Name)
	name <- return (getN nInput)
	putStrLn ("\nHello, "++name++"!")
	return name

getGender :: IO String
getGender = do
	putStrLn "Enter your gender:"
	rawInput <- getLine
	gInput <- return (read rawInput :: Gender)
	gender <- return (getG gInput)
	return gender


gameLoop :: GameState -> IO ()
--game ended early (defeat)
gameLoop oldState@(Terminated msg) = do
	putStrLn msg
--game quit
gameLoop oldState@(GameState _ _ "quit" _) = do
	putStrLn "Game stopped."
--game ended on time (victory)
gameLoop oldState@(GameState (World (home:(Location _ _ _ _ (Just (Enemy _ False)) _):locs) _) _ _ _) = do 
	putStrLn "You Win!!!"
--lost a fight
gameLoop oldState@(GameState _ (Player _ _ _ _ False) msg _) = do
	putStrLn msg
	gameLoop (Terminated "You have been killed!")
--time expired
gameLoop oldState@(GameState _ _ _ 25) = do
	gameLoop (Terminated "Time has expired!")
--main loop
gameLoop oldState = do
	displayState oldState
	rawInput <- getLine
	cmd <- return (read rawInput :: Command)
	newState <- updateState oldState cmd
	gameLoop newState
	

displayState :: GameState -> IO ()
displayState state = do 
	putStrLn (message state)
	putStrLn ("Moves: "++(show (numTurns state)))


updateState :: GameState -> Command -> IO GameState
updateState state Quit = do return (quit state)
updateState state (Answer ans) = do return (answer state ans)
updateState state Examine = do return (examine state)
updateState state Look = do return (look state)
updateState state Check = do return (checkStatus state)
updateState state Inventory = do return (inventory state)
updateState state (Take iName) = do return (pickUp state iName)
updateState state (Drop iName) = do return (ditch state iName)
updateState state (Use iName) = do return (use state iName)
updateState state (Move dr) = do return (move state dr)
updateState state Status = do 
	putStrLn (show state)
	return state
updateState state Help = do return (help state)
updateState state Invalid = do return (invalid state)