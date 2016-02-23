import Extras
--version0.1

main :: IO ()
main = textAdventure

textAdventure :: IO ()
textAdventure = do
	intro 
	name <- getName
	gameLoop name
	--outro


intro :: IO ()
intro = do putStrLn "WELCOME!!!"

getName :: IO String
getName = do
	putStrLn "Enter your name (in quotation marks):\n"
	rawInput <- getLine
	name <- return (read rawInput :: String)
	putStrLn ("Hello, "++name++"!")
	return name

gameLoop :: String -> IO ()
gameLoop name = do
	putStrLn ("loop started with name: "++name)
	putStrLn (test)
