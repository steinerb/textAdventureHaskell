module GameData where
import Types
--version0.9

maxMoves :: Int
maxMoves = 25

gameWorld :: String -> World
gameWorld g = World (locations g) connections

locations :: String -> [Location]
locations g = if (head g) == 'F' || (head g) == 'f' || (head g) == 'G' || (head g) == 'g' || (head g) == 'W' || (head g) == 'w' then
			[
				Location 0 "Home" "a small brick house, centered in a large field." [(Item 0 "Note" "To our dearest daughter,\n\n\tWe heard strange noises in the forrest last night. \nWe've gone to check it out, and should be back by sunrise.\n\tLove Dad" (-1))] Nothing False,
				Location 1 "Forrest" "your family entangled in a massive web, crawling with spiders! \nYour mom yells over to you, and explains that an oracle informed her of a prophecy that tells of a \nmighty warrioress who would save her family weilding a magic sword. \nYou notice a train ticket on the ground that slipped from her pocket. \nYou know deep within your heart what you must do." [(Item 1 "Ticket" "a train ticket." 1)] (Just (Enemy "Spiders" True)) False,
				Location 2 "TownStation" "a small town station with the tracks heading only north. \nThe rugged lands of Shaolin await!" [] Nothing False,
				Location 3 "CityStation" "a hustling and bustling metropolis. \nSo much to see in so little time!" [] Nothing False,
				Location 4 "Street" "a creepy-looking man, standing behind multiple sad-looking women. \nIn his blindspot you notice an old, rusty sword. \nCould it be?" [(Item 2 "RustySword" "an old, beaten up sword." (-1))] (Just (Enemy "Pimp" True)) False,
				Location 5 "Gate" "a tall, iron gate. \nIn front of it is an armored man weilding a spear and facing away from you. \nThe element of surprise may be on your side." [] (Just (Enemy "GateKeeper" True)) False,
				Location 6 "Monestary" "a beautiful looking temple with a high ceiling and stain glass windows. \nIn the center of the room is the most magnificent sword you have ever laid your eyes on." [(Item 3 "WuTangSword" "a magnificent blade engulfed in flames!" (-1))] Nothing False,
				Location 7 "Trapped" "nothing but black. You cannot escape" [] (Just (Enemy "GateKeeper" True)) False
			]
			else
			[
				Location 0 "Home" "a small brick house, centered in a large field." [(Item 0 "Note" "To our dearest son,\n\n\tWe heard strange noises in the forrest last night. \nWe've gone to check it out, and should be back by sunrise.\n\tLove Dad" (-1))] Nothing False,
				Location 1 "Forrest" "your family entangled in a massive web, crawling with spiders! \nYour mom yells over to you, and explains that an oracle informed her of a prophecy that tells of a \nmighty warrior who would save his family weilding a magic sword. \nYou notice a train ticket on the ground that slipped from her pocket. \nYou know deep within your heart what you must do." [(Item 1 "Ticket" "a train ticket." 1)] (Just (Enemy "Spiders" True)) False,
				Location 2 "TownStation" "a small town station with the tracks heading only north. \nThe rugged lands of Shaolin await!" [] Nothing False,
				Location 3 "CityStation" "a hustling and bustling metropolis. \nSo much to see in so little time!" [] Nothing False,
				Location 4 "Street" "a flashy-looking man, standing behind multiple beautiful women. \nIn his blindspot you notice an old, rusty sword. \nCould it be?" [(Item 2 "RustySword" "an old, beaten up sword." (-1))] (Just (Enemy "Pimp" True)) False,
				Location 5 "Gate" "a tall, iron gate. \nIn front of it is an armored man weilding a spear and facing away from you. \nThe element of surprise may be on your side." [] (Just (Enemy "GateKeeper" True)) False,
				Location 6 "Monestary" "a beautiful looking temple with a high ceiling and stain glass windows. \nIn the center of the room is the most magnificent sword you have ever laid your eyes on." [(Item 3 "WuTangSword" "a magnificent blade engulfed in flames!" (-1))] Nothing False,
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