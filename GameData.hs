module GameData where
import Types
--version0.7


testState :: GameState
testState = GameState (gameWorld "Female") (Player "Test" "Female" 0 [] True) "You wake up to an unusually quiet household.\nAfter looking around for quite some time, you realize your family is nowhere to be found.\nEverything in the house is exactly as they would have left it, except for a note on the table." 0
--testState = Terminated "This is a test! Your entire life is a test! Everything you know as you know it is a lie!"


gameWorld :: String -> World
gameWorld g = World (locations g) connections

locations :: String -> [Location]
locations g = if (head g) == 'F' || (head g) == 'f' || (head g) == 'G' || (head g) == 'g' || (head g) == 'W' || (head g) == 'w' then
			[
				Location 0 "Home" "a Home Desc." [(Item 0 "Note" "Note Desc [FEMALE]" (-1))] Nothing False,
				Location 1 "Forrest" "a Forrest Desc." [(Item 1 "Ticket" "Ticket Desc" 1)] (Just (Enemy "Spiders" True)) False,
				Location 2 "TownStation" "a TownStation Desc." [] Nothing False,
				Location 3 "CityStation" "a CityStation Desc." [] Nothing False,
				Location 4 "Street" "a Street Desc." [(Item 2 "RustySword" "RustySword Desc" (-1))] (Just (Enemy "Pimp" True)) False,
				Location 5 "Gate" "a Gate Desc." [] (Just (Enemy "GateKeeper" True)) False,
				Location 6 "Monestary" "a Monestary Desc." [(Item 3 "WuTangSword" "WuTangSword Desc" (-1))] Nothing False,
				Location 7 "Trapped" "nothing but black. You cannot escape" [] (Just (Enemy "GateKeeper" True)) False
			]
			else
			[
				Location 0 "Home" "a Home Desc." [(Item 0 "Note" "Note Desc [MALE]" (-1))] Nothing False,
				Location 1 "Forrest" "a Forrest Desc." [(Item 1 "Ticket" "Ticket Desc" 1)] (Just (Enemy "Spiders" True)) False,
				Location 2 "TownStation" "a TownStation Desc." [] Nothing False,
				Location 3 "CityStation" "a CityStation Desc." [] Nothing False,
				--Location 4 "Street" "a Street Desc." [(Item 2 "RustySword" "RustySword Desc" (-1))] (Just (Enemy "Pimp" True)) False,
				Location 4 "Street" "an old, rusty sword on the ground. Could it be?" [(Item 2 "RustySword" "RustySword Desc" (-1))] (Just (Enemy "Pimp" True)) False,
				Location 5 "Gate" "a Gate Desc." [] (Just (Enemy "GateKeeper" True)) False,
				Location 6 "Monestary" "a Monestary Desc." [(Item 3 "WuTangSword" "WuTangSword Desc" (-1))] Nothing False,
				Location 7 "Trapped" "nothing but black. You cannot escape" [] (Just (Enemy "GateKeeper" True)) False
			]

connections :: [[Con]]
connections = 
			[
				[0, 2, 4, 0, 0, 0, 0],
				[4, 0, 0, 0, 0, 0, 0],
				[2, 0, 0, 1, 0, 0, 0],
				[0, 0, 3, 0, 1, 2, 0],
				[0, 0, 0, 3, 0, 0, 0],
				[0, 0, 0, 4, 0, 0, 1],
				[0, 0, 0, 0, 0, 3, 0],
				[0, 0, 0, 0, 0, 0, 0]
			]
