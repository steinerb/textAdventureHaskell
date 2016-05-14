module Types where
import Data.Char
--version1.0


type Con = Int
type ID = Int
type Message = String

data World = World {worldLocs :: [Location], worldCons :: [[Con]]} deriving (Show)
data Location = Location {locID :: ID, locName :: String, locDesc :: String, locItem :: [Item], locEnemy :: Maybe Enemy, searched :: Bool} deriving (Show)
data Item = Item {itemID :: ID, itemName :: String, itemDesc :: String, uses :: Int} deriving (Eq, Show)
data Enemy = Enemy {enemyName :: String, isAlive :: Bool} deriving (Show)

data Player = Player {playerName :: String, playerGender :: String, playerLoc :: ID, stuff :: [Item], stillAlive :: Bool} deriving (Show)

data GameState = GameState {theWorld :: World, thePlayer :: Player, message :: Message, numTurns :: Int} 
			   | Terminated Message
					deriving (Show)

data Name = Name {getN :: String} deriving (Show, Eq)
data Gender = Gender {getG :: String} deriving (Show, Eq)		  

data Dir = North
		 | East
		 | South
		 | West
			deriving (Show, Eq)

data Command = Quit
			 | Answer Int
			 | Examine
			 | Look
			 | Check
			 | Inventory
			 | Take String
			 | Drop String
			 | Use String
			 | Move Dir
			 | Status
			 | Help
			 | Invalid
			 	deriving (Eq, Show)

instance Read Name where
	readsPrec _ s = [(Name (show s), "")]

instance Read Gender where
	readsPrec _ s
		| take 1 (map toLower s) == "m" = [(Gender "Male", "")]
		| take 1 (map toLower s) == "b" = [(Gender "Male", "")]
		| take 1 (map toLower s) == "f" = [(Gender "Female", "")]
		| take 1 (map toLower s) == "w" = [(Gender "Female", "")]
		| take 1 (map toLower s) == "g" = [(Gender "Female", "")]

instance Read Command where
	readsPrec _ s
		| map toLower s == "q" = [(Quit, "")]
		| s == "1" = [(Answer 1, "")]
		| s == "2" = [(Answer 2, "")]
		| s == "3" = [(Answer 3, "")]
		| s == "4" = [(Answer 4, "")]
		| map toLower s == "x" = [(Examine, "")]
		| map toLower s == "l" = [(Look, "")]
		| map toLower s == "c" = [(Check, "")]
		| map toLower s == "i" = [(Inventory, "")]
		| take 2 (map toLower s) == "t " = [(Take ((words s)!!1), "")]
		| take 2 (map toLower s) == "d " = [(Drop ((words s)!!1), "")]
		| take 2 (map toLower s) == "u " = [(Use ((words s)!!1), "")]
		| map toLower s == "g" = [(Status, "")]
		| map toLower s == "h" = [(Help, "")]
		| map toLower s == "n" = [((Move North), "")]
		| map toLower s == "e" = [((Move East), "")]
		| map toLower s == "s" = [((Move South), "")]
		| map toLower s == "w" = [((Move West), "")]
		| s == s = [(Invalid, "")]
		

class Desc a where
	name :: a -> String
	desc :: a -> String

instance Desc Location where
	name (Location _ n _ _ _ _) = n
	desc (Location _ _ d _ _ _) = d

instance Desc Item where
	name (Item _ n _ _) = n
	desc (Item _ _ d _) = d

instance Desc Player where
	name (Player n _ _ _ _) = n
	desc (Player n g _ i _) = "Name: "++n++"\nGender: "++g++"\nInventory: "++(unwords (map name i))



class Container c where
	contents :: c -> [Item]
	acquire :: c -> Item -> c
	release :: c -> Item -> c
	contains :: c -> Item -> Bool
	isEmpty :: c -> Bool


instance Container Player where
	contents (Player _ _ _ is _) = is
	acquire (Player n g l is a) i = Player n g l (i:is) a
	release (Player n g l is a) i = Player n g l (filter (/=i) is) a
	contains (Player _ _ _ is _) i = i `elem` is
	isEmpty (Player _ _ _ is _) = if is == [] then True else False

instance Container Location where
	contents (Location _ _ _ is _ _) = is
	acquire (Location id n d is e s) i = Location id n d (i:is) e s
	release (Location id n d is e s) i = Location id n d (filter (/=i) is) e s
	contains (Location _ _ _ is _ _) i = i `elem` is
	isEmpty (Location _ _ _ is _ _) = if is == [] then True else False