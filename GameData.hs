module GameData where
import Types
--version0.3


testState :: GameState
testState = GameState gameWorld (Player "Test" 0 [] True) "You wake up to an unusually quiet household.\nAfter looking around for quite some time, you realize your family is nowhere to be found.\nEverything in the house is exactly as they would have left it, except for a note on the table." 0

gameWorld :: World
gameWorld = World locations connections

locations :: [Location]
locations = [
				Location 0 "Home" "Home Desc" [(Item 0 "Note")] Nothing,
				Location 1 "Forrest" "Forrest Desc" [(Item 1 "Ticket")] (Just (Enemy "Spiders" True)),
				Location 2 "Station1" "Station1 Desc" [] Nothing,
				Location 3 "Station2" "Station2 Desc" [] Nothing,
				Location 4 "Street" "Street Desc" [(Item 2 "Weapon")] (Just (Enemy "Hobo" True)),
				Location 5 "Gate" "Gate Desc" [] (Just (Enemy "GateKeeper" True)),
				Location 6 "Monestary" "Monestary Desc" [(Item 3 "WuTangSword")] Nothing
			]

connections :: [[Con]]
connections = 
			[
				[0, 1, 1, 0, 0, 0, 0],
				[1, 0, 0, 0, 0, 0, 0],
				[1, 0, 0, 1, 0, 0, 0],
				[0, 0, 1, 0, 1, 1, 0],
				[0, 0, 0, 1, 0, 0, 0],
				[0, 0, 0, 1, 0, 0, 1],
				[0, 0, 0, 0, 0, 1, 0]
			]