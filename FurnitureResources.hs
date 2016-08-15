module FurnitureResources where

import System.Random
import System.IO.Unsafe

-- randomly chosen from the range [0 .. x]
randomZeroToX :: Int -> Int
randomZeroToX x = unsafePerformIO (getStdRandom (randomR (0, x)))

furniture2 = ["couch","table","lamp","tv","e"]

room1 = [["e","e","e","lamp","table"],["tv","e","table","e","couch"],["tv","e","table","e","couch"],["tv","e","e","e","couch"],["e","e","e","lamp","table"]]
room2 = [["couch","couch","couch","table","lamp"],["couch","e","e","e","e"],["couch","e","table","e","tv"],["couch","e","table","e","tv"],["table","e","e","e","e"]]
room3 = [["e","tv","tv","tv"],["e","e","e","e"],["e","table","table","table"],["table","couch","couch","couch"]]

training =  [room1,room2,room3]
