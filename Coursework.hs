-- UP918631

import Data.List
import System.IO

type Place = (String, (Float, Float), [Int])
testData :: [Place]
			
testData = [("London", (51.5, -0.1), [0,0,5,8,8,0,0]), 
            ("Cardiff", (51.5, -3.2), [12,8,15,0,0,0,2]), 
            ("Norwich", (52.6, 1.3), [0,6,5,0,0,0,3]), 
            ("Birmingham", (52.5, -1.9), [0,2,10,7,8,2,2]),         
            ("Liverpool", (53.4, -3.0), [8,16,20,3,4,9,2]), 
            ("Hull", (53.8, -0.3), [0,6,5,0,0,0,4]), 
            ("Newcastle", (55.0, -1.6), [0,0,8,3,6,7,5]), 
            ("Belfast", (54.6, -5.9), [10,18,14,0,6,5,2]), 
            ("Glasgow", (55.9, -4.3), [7,5,3,0,6,5,0]), 
            ("Plymouth", (50.4, -4.1), [4,9,0,0,0,6,5]), 
            ("Aberdeen", (57.1, -2.1), [0,0,6,5,8,2,0]), 
            ("Stornoway", (58.2, -6.4), [15,6,15,0,0,4,2]), 
            ("Lerwick", (60.2, -1.1), [8,10,5,5,0,0,3]),
            ("St, Helier", (49.2, -2.1), [0,0,0,0,6,10,0])]
		  

---------------------------------------- Task 1 -----------------------------------------------

-- Use: > displayNames testData


displayNames :: [Place] -> [String]
displayNames nameList = [name | (name, _, _) <- nameList ]


----------------------------------------- Task 2------------------------------------------------

-- -- Use: > returnAverage testData "London"


returnAverage :: [Place] -> String -> Float
returnAverage [] _ = 0
returnAverage (p:ps) find
    | find == name = avg rainfall
    | otherwise = returnAverage ps find
    where
        (name, _, rainfall) = p
        fi = fromIntegral 
        avg :: [Int] -> Float
        avg xs = (fi . sum $ xs) / (fi . length $ xs)

		
--------------------------------------- Task 3 -------------------------------------------------

-- -- Usage: > putStrLn(placesToString testData)


fillLeft c n s = s ++ replicate (n - length s) c

placesToString :: [Place] -> String
placesToString places = unlines [intercalate "  |  " [fillLeft ' ' 10 location, 
                                                      fillLeft ' ' 3(show a), 
                                                      fillLeft ' ' 3(show b),
                                                      fillLeft ' ' 3(show c),
                                                      fillLeft ' ' 3(show d),
                                                      fillLeft ' ' 3(show e),
                                                      fillLeft ' ' 3(show f), 
                                                      fillLeft ' ' 3(show g)]
                                                      | (location, _, [a,b,c,d,e,f,g] ) <- places ]
							
								
--------------------------------------- Task 4 ----------------------------------------------------

-- Usage: > returnDryDays testData 5


returnDryDays :: [Place] -> Int -> [String]
returnDryDays testData daysAgo = [ name | (name, _, [a,b,c,d,e,f,g]) <- testData, 
                                                                                                     daysAgo == 7 && g == 0 || 
																									 daysAgo == 6 && f == 0 ||
																									 daysAgo == 5 && e == 0 ||
																									 daysAgo == 4 && d == 0 ||
																									 daysAgo == 3 && c == 0 ||
																									 daysAgo == 2 && b == 0 ||
																									 daysAgo == 1 && a == 0]
																					
																																					
--------------------------------------- Task 5 ----------------------------------------------------

-- Usage: > updateAllRainfall testData [22,14,0,52,25,11,9,11,12,22,12,12,7,2]


getIndex :: [Place] -> Place -> Int
getIndex (x:xs) 
      s | not(x == s) = 1 + (getIndex xs s)
        | otherwise = 0

updateAllRainfall :: [Place] -> [Int] -> [Place]
updateAllRainfall places newNums = map (updateRainfall newNums places) places

updateRainfall :: [Int] -> [Place] -> Place -> Place
updateRainfall newNums places (name, coordinates, [a,b,c,d,e,f,g]) = (name, coordinates, [newNums!!x,b,c,d,e,f,g])
                                                                     where x = getIndex places (name, coordinates, [a,b,c,d,e,f,g])

--------------------------------------- Task 6 -----------------------------------------------------

-- Usage: > updateList testData ("Portsmouth", (51.5, -0.1), [12, 24, 36, 48, 55, 66, 70]) ("Lerwick", (60.2, -1.1), [8, 10, 5, 5, 0, 0, 3])


updateList :: [Place] -> Place -> Place -> [Place]
updateList places newPlace oldPlace = map (updateRecord newPlace places oldPlace) places 

updateRecord :: Place -> [Place] -> Place -> Place -> Place
updateRecord newPlace places oldPlace(name, coordinates, [a,b,c,d,e,f,g]) 
   | (name, coordinates, [a,b,c,d,e,f,g]) == oldPlace = (newPlace)
   | otherwise = (name, coordinates, [a,b,c,d,e,f,g])
                                                                    
																	        
--------------------------------------- Task 7 -----------------------------------------------------

-- Usage: > closestDry (51.5, -0.1) testData

distance :: (Float, Float) -> (Float, Float) -> Float
distance (n1,e1) (n2,e2) = (n2 - n1)^2 + (e2 - e1)^2

distanceToTwo :: (Float, Float) -> Place -> Place -> Place
distanceToTwo from to1@(_,location1,_) to2@(_,location2,_)
    | distance from location1 > distance from location2 = to2
    | otherwise = to1


closestDry :: (Float, Float) -> [Place] -> Place
closestDry loc ps = foldr1 (distanceToTwo loc) [place | place@(_,_,rainfall) <- ps, rainfall !! 0 == 0]



