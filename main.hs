import FurnitureResources

--------------------
-- Training Phase --
--------------------

-- Main 1
-- statsList: generate a list of statistics according to the content
-- of the training list in the FurnitureResources.hs file
statsList :: [([Char],[[([Char],[Char],Int)]])]

statsList = statsListHelper training

-- Helper 1-a
-- statsListHelper rooms: generate a list of statistics according
-- to the content of rooms
statsListHelper [] = []
statsListHelper (room:rooms) = generate room (statsListHelper rooms)

-- Main 2
-- findFurnitureUpdate a b c stats: updates stats with the new furniture
-- relation (a, b, c) which means that b was found at position c w.r.t a
findFurnitureUpdate :: [Char] -> [Char] -> [Char] -> [([Char],[[([Char],[Char],Int)]])] ->[([Char],[[([Char],[Char],Int)]])]

findFurnitureUpdate a b c [] | c == "right" = [(a, [[(b, c, 1)], []])]
							 | c == "below" = [(a, [[], [(b, c, 1)]])]
							 | otherwise = error "position not valid"

findFurnitureUpdate a b c ((x, x_stats):rem_stats)  | a == x = (x, updateFrequency b c x_stats):rem_stats
													| otherwise = (x, x_stats):findFurnitureUpdate a b c rem_stats
-- Helper 2-a
-- updateFrequency b c stats: updates the stats list of a given furniture
-- with the new record which states that b was found at position c w.r.t
-- that furniture
updateFrequency b c [right, below]	| c == "right" = [sortFreq(updateFrequencyHelper b c right), below]
									| c == "below" = [right, sortFreq(updateFrequencyHelper b c below)]
									| otherwise = error "position not valid"

-- Helper 2-b
-- updateFrequencyHelper b c stats: updates the right/below stats list of
-- a given furniture with the new record which states that b was found at
-- position c w.r.t that furniture
updateFrequencyHelper b c [] = [(b, c, 1)]
updateFrequencyHelper b c ((x, _, freq):freqs) 	| b == x = (x, c, freq+1):freqs
 												| otherwise = (x, c, freq):updateFrequencyHelper b c freqs
-- Helper 2-c
-- insert x L: inserts the element x in a descendingly sorted list L
insert x [] = [x]
insert (o,p,x) ((o1,p1,y):ys)	| x > y = (o,p,x):(o1,p1,y):ys
								| otherwise = (o1,p1,y):insert (o,p,x) ys

-- Helper 2-d
-- sortFreq L: sorts the frequency list L descendingly using insertion sort
sortFreq [] = []
sortFreq (x:xs) = insert x (sortFreq xs)

-- Main 3
-- generate room currentStats: examines the input room and updates currentStats
generate :: [[[Char]]] -> [([Char],[[([Char],[Char],Int)]])] -> [([Char],[[([Char],[Char],Int)]])]

generate room currentStats = generateBelow room (generateRight room currentStats)

-- Helper 3-a
-- generate rows currentStats: examines the input rows and updates the right
-- stats in currentStats
generateRight [] currentStats = currentStats
generateRight (row:rows) currentStats = generateRightForRow row (generateRight rows currentStats)

-- Helper 3-b
-- generateRight rows currentStats: examines the input row and updates the right
-- stats in currentStats
generateRightForRow (_:[]) currentStats = currentStats
generateRightForRow (a:b:xs) currentStats
			= generateRightForRow (b:xs) (findFurnitureUpdate a b "right" currentStats)

-- Helper 3-c
-- generateBelow rows currentStats: examines the input row and updates the below
-- stats in currentStats
generateBelow (row:[]) currentStats = currentStats
generateBelow (row1:row2:rows) currentStats
			= generateBelow (row2:rows) (generateBelowForTwoRows row1 row2 currentStats)

-- Helper 3-d
-- generateBelowForTwoRows row1 row2 currentStats: examines row1 and row2 and updates
-- the below stats in currentStats
generateBelowForTwoRows [] [] currentStats = currentStats
generateBelowForTwoRows (furn1:furns1) (furn2:furns2) currentStats
			= generateBelowForTwoRows furns1 furns2 (findFurnitureUpdate furn1 furn2 "below" currentStats)

----------------------
-- Generation Phase --
----------------------

-- Main 4
-- furnishRoom n furn: furnish an n x n room with furn at placed at the top left
-- corner. The room is furnished with stats generated in the training phase
furnishRoom :: Int -> [Char] -> [[[Char]]]

furnishRoom n furn = furnishRoomHelper (n-1) firstRow
					where firstRow = furnishFirstRow n furn

-- Helper 4-a
-- furnishRoomHelper rows above stats: adds above (a previously furnished rows)
-- to the result room and furnish rows (the remaining rows of the room).
furnishRoomHelper 0 above = [above]
furnishRoomHelper rows (a:above) = (a:above):furnishRoomHelper (rows-1) (furnishRow furn (a:above))
 									where furn = getPossibleNeighbour [[], []] (getFurnStat a)

-- Helper 4-b
-- furnishFirstRow n furn stats: furnishes the first row of an n x n room.
-- The row will start with furn.
furnishFirstRow 0 _ = []
furnishFirstRow n furn =  furn:furnishFirstRow (n-1) (getPossibleNeighbour (getFurnStat furn) [[], []])

-- Helper 4-c
-- furnishRow furn above stats: furnishes a row with length equal to the
-- length of above (the above row). The row will start with furn.
furnishRow _ [] = []
furnishRow furn (a:above)
		= furn:furnishRow (getPossibleNeighbour (getFurnStat furn) (getFurnStat a)) above

-- Main 5
-- getFurnStat furn: retrieves the statistics of furn according to statsList
getFurnStat :: [Char] -> [[([Char],[Char],Int)]]

getFurnStat furn  = getFurnStatHelper furn statsList

-- Helper 5-a
-- getFurnStat furn stats: searches for the statistics of furn in stats
getFurnStatHelper _ [] = [[], []]
getFurnStatHelper furn ((furn1,furn1_stats):stats)| furn == furn1 = furn1_stats
 										   	 	  | otherwise = getFurnStatHelper furn stats

-- Main 6
-- getPossibleNeighbour stat1 stat2: selects randomly a furniture object to
-- be placed according to stat1 (statistics of the object to the left of the
-- current room cell) and stat2 (statistics of the object above of the current
-- room cell).
getPossibleNeighbour :: [[([Char],[Char],Int)]] -> [[([Char],[Char],Int)]] -> [Char]

getPossibleNeighbour [[], _] [_, []] = furniture2!!randomZeroToX 4
getPossibleNeighbour stat1 stat2 = selectFurn stats (randomZeroToX (sumFreq stats - 1))
								where stats = (stat1!!0)++(stat2!!1)
-- Helper 6-a
-- sumFreq stats: sums the frequencies of the objects in stats list. These
-- objects are elements of statistics list elements.
sumFreq [] = 0
sumFreq ((_, _, f):xs) = f + sumFreq xs

-- Helper 6-b
-- selectFurn stats index: finds the furniture object at the given index in stats List.
selectFurn ((a, _, f):xs) index | f < index = selectFurn xs (index-f)
								| otherwise = a
