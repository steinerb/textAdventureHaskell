module Types where
--version0.3


type Con = Int
type ID = Int
type Message = String

data World = World {worldLocs :: [Location], worldCons :: [[Con]]} deriving (Show)
data Location = Location {locID :: ID, locName :: String, locDesc :: String, locItem :: [Item], locEnemy :: Maybe Enemy} deriving (Show)
data Item = Item {itemID :: ID, itemName :: String, itemDesc :: String} deriving (Eq, Show)
data Enemy = Enemy {enemyName :: String, isAlive :: Bool} deriving (Show)

data Player = Player {playerName :: String, playerGender :: String, playerLoc :: ID, inventory :: [Item], stillAlive :: Bool} deriving (Show)

data GameState = GameState {theWorld :: World, thePlayer :: Player, message :: Message, numTurns :: Int} deriving (Show)



class Desc a where
	name :: a -> String
	desc :: a -> String

instance Desc Location where
	name (Location _ n _ _ _) = n
	desc (Location _ _ d _ _) = d

instance Desc Item where
	name (Item _ n _) = n
	desc (Item _ _ d) = d

instance Desc Player where
	name (Player n _ _ _ _) = n
	desc (Player n g _ i _) = "Name: "++n++"\nGender: "++g++"\nInventory: "++(unwords (map name i))

--instance Desc World where



{-
instance Show GameState where
	show (GameState world player message) = (show player)++"\nCurrent Location: "++(show ((worldLocs world)!!(playerLoc player)))

instance Show Location where
	show (Location id name desc item enemy) = (show name)++": "++(show desc)

instance Show Player where
	show (Player name loc inv alive) = "Name: "++(show name)++"\nInventory: "++(show inv)++"\nAlive: "++(show alive)

instance Show Item where
	show (Item id name) = (show name)
-}