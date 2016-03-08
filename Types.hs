module Types where
--version0.3


type Con = Int
type Name = String
type Desc = String
type ID = Int
type Message = String

data World = World {worldLocs :: [Location], worldCons :: [[Con]]} deriving (Show)
data Location = Location {locID :: ID, locName :: Name, locDesc :: Desc, locItem :: [Item], locEnemy :: Maybe Enemy} deriving (Show)
data Item = Item {itemID :: ID, itemName :: Name} deriving (Eq, Show)
data Enemy = Enemy {enemyName :: Name, isAlive :: Bool} deriving (Show)

data Player = Player {playerName :: Name, playerLoc :: ID, inventory :: [Item], stillAlive :: Bool} deriving (Show)

data GameState = GameState {theWorld :: World, thePlayer :: Player, message :: Message, numTurns :: Int} deriving (Show)

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