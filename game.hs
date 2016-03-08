import Types
import GameData
import Functions
--version0.3


main :: IO ()
main = textAdventure

textAdventure :: IO ()
textAdventure = do
	intro 
	name <- getName
	gameLoop (GameState gameWorld (Player name 0 [] True) "You wake up to an unusually quiet household.\nAfter looking around for quite some time, you realize your family is nowhere to be found.\nEverything in the house is exactly as they would have left it, except for a note on the table." 0)
	--outro


intro :: IO ()
intro = do putStrLn "Welcome! Please enter all commands in quotation marks."

getName :: IO String
getName = do
	putStrLn "Enter your name (in quotation marks):"
	rawInput <- getLine
	name <- return (read rawInput :: String)
	putStrLn ("\nHello, "++name++"!")
	return name

gameLoop :: GameState -> IO ()
gameLoop oldState@(GameState _ _ "quit" _) = do
	putStrLn "Game stopped."
gameLoop oldState@(GameState (World (home:(Location _ _ _ _ (Just (Enemy _ False))):locs) _) _ _ _) = do 
	putStrLn "You Win!!!"
gameLoop oldState@(GameState _ (Player _ _ _ False) _ _) = do
	putStrLn "You Lost!!!"
gameLoop oldState = do
	displayState oldState
	rawInput <- getLine
	action <- return (read rawInput :: String)
	newState <- updateState oldState action
	gameLoop newState
	

displayState :: GameState -> IO ()
displayState state = do 
	putStrLn (message state)
	putStrLn ("Moves: "++(show (numTurns state)))

updateState :: GameState -> String -> IO GameState
updateState state "Quit" = do return (quit state)
updateState state "quit" = do return (quit state)
updateState state "Q" = do return (quit state)
updateState state "q" = do return (quit state)
updateState state "L" = do return (look state)
updateState state "l" = do return (look state)
updateState state "T" = do return (pickUp state)
updateState state "t" = do return (pickUp state)
updateState state "D" = do return (ditch state)
updateState state "d" = do return (ditch state)
--updateState state "M" = do return (move state)
--updateState state "m" = do return (move state)
updateState state "status" = do 
	putStrLn (show state)
	return state
updateState state "help" = do
	help
	return state
updateState state "H" = do
	help
	return state
updateState state "h" = do
	help
	return state
updateState state other = do return (invalid state)