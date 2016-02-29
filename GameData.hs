module GameData where
import Types
--version0.1



gameWorld :: World
gameWorld = World locations connections

locations :: [Location]
locations = [
				Location 0 "Home" "Home Desc" (Just (Item 0 "Note")) Nothing,
				Location 1 "Forrest" "Forrest Desc" (Just (Item 1 "Ticket")) (Just (Enemy "Spiders" True)),
				Location 2 "Station1" "Station1 Desc" Nothing Nothing,
				Location 3 "Station2" "Station2 Desc" Nothing Nothing,
				Location 4 "Street" "Street Desc" (Just (Item 2 "Weapon")) (Just (Enemy "Hobo" True)),
				Location 5 "Gate" "Gate Desc" Nothing (Just (Enemy "GateKeeper" True)),
				Location 6 "Monestary" "Monestary Desc" (Just (Item 3 "WuTangSword")) Nothing
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

initialItems :: [Item]
initialItems = [Item 0 "Note", Item 1 "Ticket", Item 2 "Weapon", Item 3 "WuTangSword"]

initialMaybeItems :: [Maybe Item]
initialMaybeItems = 
	[
		Just (Item 0 "Note"), 
		Just (Item 1 "Ticket"), 
		Nothing, 
		Nothing, 
		Just (Item 2 "Weapon"), 
		Nothing, 
		Just (Item 3 "WuTangSword")
	]