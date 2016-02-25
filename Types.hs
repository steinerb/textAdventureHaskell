module Types where
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
