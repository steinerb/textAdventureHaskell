Themes&Backstory: Player must enter the Rugged Lands of Shaolin to find the legendary Wu-Tang Sword in order save his homeland from spiders.

Locations: Locations include the player's home, a train station (or two), a street, a gate, the shaolin monestary, and a spider infested forrest.

Items&Obstacles: The player will need a train ticket to use the train, find a weapon to slay someone in the street, and retrieve the Wu Tang Sword to defeat the spiders at home.

Goals&Obstacles: The player must retrieve the legendary Wu-Tang Sword to save his homeland. The second to last obstacle that hasn't been stated above is answering a riddle at the gate where the sword is. The last Obstacle that hasn't been stated above is returning home with the sword to defeat the spiders.

Essential Types&Classes: Types include (but aren't limited to and may change):
	World :: [Location] [[connection]]
	connection :: Bool
	Location :: Int String String Item Enemy
	Item :: Int
	--OR
	type Item = option 1 | option 2 | ...
	Enemy :: String Bool
	Player :: String Location Item

Functional Design: Because my game ends with the death of the spiders, the boolean isAlive property I'm planning on giving all enemies could be used to keep the game going until the spiders are dead.
From a main function (or a function being run from the main function), the "do" keyword could be used to get the player's name, play an intro, run the game loop, and then play an outro.