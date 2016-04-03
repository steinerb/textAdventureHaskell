module Types where
import Data.Char
--version0.5


type Con = Int
type ID = Int
type Message = String

data World = World {worldLocs :: [Location], worldCons :: [[Con]]} deriving (Show)
data Location = Location {locID :: ID, locName :: String, locDesc :: String, locItem :: [Item], locEnemy :: Maybe Enemy} deriving (Show)
data Item = Item {itemID :: ID, itemName :: String, itemDesc :: String} deriving (Eq, Show)
data Enemy = Enemy {enemyName :: String, isAlive :: Bool} deriving (Show)

data Player = Player {playerName :: String, playerGender :: String, playerLoc :: ID, inventory :: [Item], stillAlive :: Bool} deriving (Show)

data GameState = GameState {theWorld :: World, thePlayer :: Player, message :: Message, numTurns :: Int} deriving (Show)

data Dir = Home
		 | Forrest
		 | TownStation
		 | CityStation
		 | Street
		 | Gate
		 | Monestary
		 	deriving (Show, Eq)

data Command = Quit
			 | Look
			 | Take
			 | Drop
			 | Move Dir
			 | Status
			 | Help
			 | Invalid
			 	deriving (Eq, Show)


class DirProperties a where
	toString :: a -> String
	getID :: a -> ID

instance DirProperties Dir where
	toString Home = "Home"
	toString Forrest = "Forrest"
	toString TownStation = "TownStation"
	toString CityStation = "CityStation"
	toString Street = "Street"
	toString Gate = "Gate"
	toString Monestary = "Monestary"
	getID Home = 0
	getID Forrest = 1
	getID TownStation = 2
	getID CityStation = 3
	getID Street = 4
	getID Gate = 5
	getID Monestary = 6


instance Read Command where
	readsPrec _ s
		| map toLower s == "q" = [(Quit, "")]
		| map toLower s == "l" = [(Look, "")]
		| map toLower s == "t" = [(Take, "")]
		| map toLower s == "d" = [(Drop, "")]
		| map toLower s == "s" = [(Status, "")]
		| map toLower s == "h" = [(Help, "")]
		| take 3 (map toLower s) == "hom" = [((Move Home), "")]
		| take 3 (map toLower s) == "for" = [((Move Forrest), "")]
		| take 3 (map toLower s) == "tow" = [((Move TownStation), "")]
		| take 3 (map toLower s) == "cit" = [((Move CityStation), "")]
		| take 3 (map toLower s) == "str" = [((Move Street), "")]
		| take 3 (map toLower s) == "gat" = [((Move Gate), "")]
		| take 3 (map toLower s) == "mon" = [((Move Monestary), "")]
		| map toLower s == map toLower s = [(Invalid, "")]
		

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
class Container a where
	contents :: c -> [Item]
	acquire :: c -> Item -> c
	release :: c -> Item -> c
	contains :: c -> Item -> Bool
	isEmpty :: c -> Bool
-}