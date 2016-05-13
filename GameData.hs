module GameData where
import Types
--version0.7

maxMoves :: Int
maxMoves = 25

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
				Location 7 "Trapped" "nothing but black. There is no escape." [] (Just (Enemy "GateKeeper" True)) False
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

question :: String
question = 
	"\tWhich member of the Wu Tang Clan is no longer with us?\n"++
				"\t------------------------------------------------------------\n"++
				"\t\t(1) Inspecta Deck\n"++
				"\t\t(2) Ol' Dirty Bastard\n"++
				"\t\t(3) RZA\n"++
				"\t\t(4) GZA"