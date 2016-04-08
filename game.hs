import Types
import GameData
import Functions
--version0.5


main :: IO ()
main = textAdventure

textAdventure :: IO ()
textAdventure = do
	intro
	name <- getName
	gender <- getGender
	gameLoop (GameState (gameWorld gender) (Player name gender 0 [] True) "You wake up to an unusually quiet household.\nAfter looking around for quite some time, you realize your family is nowhere to be found.\nEverything in the house is exactly as they would have left it, except for a note on the table." 0)
	--outro


intro :: IO ()
intro = do putStrLn "Welcome!!!"

getName :: IO String
getName = do
	putStrLn "Enter your name (in quotation marks):"
	rawInput <- getLine
	name <- return (read rawInput :: String)
	putStrLn ("\nHello, "++name++"!")
	return name

getGender :: IO String
getGender = do
	putStrLn "Enter your gender (in quotation marks):"
	rawInput <- getLine
	gender <- return (read rawInput :: String)
	return gender

gameLoop :: GameState -> IO ()
gameLoop oldState@(GameState _ _ "quit" _) = do
	putStrLn "Game stopped."
gameLoop oldState@(GameState (World (home:(Location _ _ _ _ (Just (Enemy _ False))):locs) _) _ _ _) = do 
	putStrLn "You Win!!!"
gameLoop oldState@(GameState _ (Player _ _ _ _ False) _ _) = do
	putStrLn "You Lost!!!"
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
updateState state Look = do return (look state)
updateState state (Take iName) = do return (pickUp state iName)
updateState state (Drop iName) = do return (ditch state iName)
updateState state (Move dr) = do return (move state dr)
updateState state Status = do 
	putStrLn (show state)
	return state
updateState state Help = do
	help
	return state
updateState state Invalid = do return (invalid state)