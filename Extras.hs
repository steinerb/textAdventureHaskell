module Extras where 
--version0.1


test :: String
test = "Second module successfully imported!"

--types
type Con = Int
type Name = String
type Desc = String
type ID = Int

data World = World {worldLocs :: [Location], worldCons :: [[Con]] } deriving (Show)
data Location = Location {locID :: ID, locName :: Name, locDesc :: Desc, locItem :: Maybe Item, locEnemy :: Maybe Enemy} deriving (Show)
data Item = Item {itemID :: ID, itemName :: Name} deriving (Show)
data Enemy = Enemy {enemyName :: Name, isAlive :: Bool} deriving (Show)



gameWorld :: World
gameWorld = World locations connections

locations :: [Location]
locations = [
				Location 0 "Home" "Desc" Nothing Nothing,
				Location 1 "Forrest" "Desc" (Just (Item 0 "Ticket")) (Just (Enemy "Spiders" True)),
				Location 2 "Station1" "Desc" Nothing Nothing,
				Location 3 "Station2" "Desc" Nothing Nothing,
				Location 4 "Street" "Desc" (Just (Item 1 "Weapon")) (Just (Enemy "Hobo" True)),
				Location 5 "Gate" "Desc" Nothing (Just (Enemy "GateKeeper" True)),
				Location 6 "Monestary" "Desc" (Just (Item 2 "WuTangSword")) Nothing
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

items :: [Item]
items = [Item 0 "Ticket", Item 1 "Weapon", Item 2 "WuTangSword"]

maybeItems :: [Maybe Item]
maybeItems = 
	[
		Nothing, 
		Just (Item 0 "Ticket"), 
		Nothing, 
		Nothing, 
		Just (Item 1 "Weapon"), 
		Nothing, 
		Just (Item 2 "WuTangSword")
	]