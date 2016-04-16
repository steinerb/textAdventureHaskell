module GameData where
import Types
--version0.7


testState :: GameState
testState = GameState (gameWorld "Female") (Player "Test" "Female" 0 [] True) "You wake up to an unusually quiet household.\nAfter looking around for quite some time, you realize your family is nowhere to be found.\nEverything in the house is exactly as they would have left it, except for a note on the table." 0


gameWorld :: String -> World
gameWorld g = World (locations g) connections

--added the "searched" boolean to Locations
locations :: String -> [Location]
locations g = if (head g) == 'F' || (head g) == 'f' || (head g) == 'G' || (head g) == 'g' || (head g) == 'W' || (head g) == 'w' then
			[
				Location 0 "Home" "Home Desc.\n\tTo your East is a Forrest.\n\tTo your West is a TownStation." [(Item 0 "Note" "Note Desc [FEMALE]")] Nothing False,
				Location 1 "Forrest" "Forrest Desc.\n\tTo your West is a House." [(Item 1 "Ticket" "Ticket Desc")] (Just (Enemy "Spiders" True)) False,
				Location 2 "TownStation" "TownStation Desc.\n\tTo your North is a CityStation.\n\tTo your East is a House." [] Nothing False,
				Location 3 "CityStation" "CityStation Desc.\n\tTo your North is a Street.\n\tTo your east is a Gate.\n\tTo your South is a TownStation." [] Nothing False,
				Location 4 "Street" "Street Desc.\n\tTo your South is a CityStation." [(Item 2 "Weapon" "Weapon Desc")] (Just (Enemy "Hobo" True)) False,
				Location 5 "Gate" "Gate Desc.\n\tTo your North is a Monestary.\n\tTo your West is a CityStation." [] (Just (Enemy "GateKeeper" True)) False,
				Location 6 "Monestary" "Monestary Desc.\n\tTo your South is a Gate." [(Item 3 "WuTangSword" "WuTangSword Desc")] Nothing False
			]
			else
			[
				Location 0 "Home" "Home Desc.\n\tTo your East is a Forrest.\n\tTo your West is a TownStation." [(Item 0 "Note" "Note Desc [MALE]")] Nothing False,
				Location 1 "Forrest" "Forrest Desc.\n\tTo your West is a House." [(Item 1 "Ticket" "Ticket Desc")] (Just (Enemy "Spiders" True)) False,
				Location 2 "TownStation" "TownStation Desc.\n\tTo your North is a CityStation.\n\tTo your East is a House." [] Nothing False,
				Location 3 "CityStation" "CityStation Desc.\n\tTo your North is a Street.\n\tTo your east is a Gate.\n\tTo your South is a TownStation." [] Nothing False,
				Location 4 "Street" "Street Desc.\n\tTo your South is a CityStation." [(Item 2 "Weapon" "Weapon Desc")] (Just (Enemy "Hobo" True)) False,
				Location 5 "Gate" "Gate Desc.\n\tTo your North is a Monestary.\n\tTo your West is a CityStation." [] (Just (Enemy "GateKeeper" True)) False,
				Location 6 "Monestary" "Monestary Desc.\n\tTo your South is a Gate." [(Item 3 "WuTangSword" "WuTangSword Desc")] Nothing False
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