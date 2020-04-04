-- UP918631

import Data.List
import System.IO





type Place = (String, Double, Double, Int, Int, Int, Int, Int, Int ,Int)
testData :: [Place]

testData = [("London", 51.5, -0.1, 0, 0, 5, 8, 8, 0, 0),
            ("Cardiff", 51.5, -3.2, 12, 8, 15, 0, 0, 0, 2),
            ("Norwich", 52.6, 1.3, 0, 6, 5, 0, 0, 0, 3),
            ("Birmingham", 52.5, -1.9, 0, 2, 10, 7, 8, 2, 2),
            ("Liverpool", 53.4, -3.0, 8, 16, 20, 3, 4, 9, 2),
            ("Hull", 53.8, -0.3, 0, 6, 5, 0, 0, 0, 4),
            ("Newcastle", 55.0, -1.6, 0, 0, 8, 3, 6, 7, 5),
            ("Belfast", 54.6, -5.9, 10, 18, 14, 0, 6, 5, 2),
            ("Glasgow", 55.9, -4.3, 3, 5, 3, 0, 6, 5, 0),
            ("Plymouth", 50.4, -4.1, 4, 9, 0, 0, 0, 6, 5),
            ("Aberdeen", 57.1, -2.1, 0, 0, 6, 5, 8, 2, 0),
            ("Stornoway", 58.2, -6.4, 15, 6, 15, 0, 0, 4, 2),
            ("Lerwick", 60.2, -1.1, 8, 10, 5, 5, 0, 0, 3),
            ("St Helier", 49.2, -2.1, 0, 0, 0, 0, 6, 10, 0)]
		 
		 

---------------------------------------- Task 1 -----------------------------------------------
-- Use: > displayNames testData

displayNames :: [Place] -> [String]
displayNames nameList = [name | (name, _, _, _, _, _, _, _, _, _) <- nameList ]


---------------------------------------- Task 2------------------------------------------------
-- Use: > returnAverage testData "London"

returnAverage :: [Place] -> String -> Float
returnAverage [] _ = 0
returnAverage (p:ps) find
    | find == name = avg [a,b,c,d,e,f,g]
    | otherwise = returnAverage ps find
    where
        (name, _,_ , a,b,c,d,e,f,g) = p
        fi = fromIntegral 
        avg :: [Int] -> Float
        avg xs = (fi . sum $ xs) / (fi . length $ xs)


		
--------------------------------------- Task 3 -------------------------------------------------
-- Usage: > putStrLn(placesToString testData)

fillLeft c n s = s ++ replicate (n - length s) c

placesToString :: [Place] -> String
placesToString places = unlines [intercalate "  |  " [fillLeft ' ' 10 location, 
                                                      fillLeft ' ' 3(show mon), 
                                                      fillLeft ' ' 3(show tue),
                                                      fillLeft ' ' 3(show wed),
                                                      fillLeft ' ' 3(show thu),
                                                      fillLeft ' ' 3(show fri),
                                                      fillLeft ' ' 3(show sat), 
                                                      fillLeft ' ' 3(show sun)]
                                                      | (location, _, _, mon, tue, wed, thu, fri, sat, sun ) <- places ]

								
								
--------------------------------------- Task 4 ----------------------------------------------------
-- Usage: > returnDryDays testData daysAgo

returnDryDays :: [Place] -> Int -> [String]
returnDryDays testData daysAgo = [ name | (name, _, _, mon, tue, wed, thu, fri, sat, sun) <- testData, 
                                                                                                     daysAgo == 7 && sun == 0 || 
																									 daysAgo == 6 && sat == 0 ||
																									 daysAgo == 5 && fri == 0 ||
																									 daysAgo == 4 && thu == 0 ||
																									 daysAgo == 3 && wed == 0 ||
																									 daysAgo == 2 && tue == 0 ||
																									 daysAgo == 1 && mon == 0]
																					
																			
																			
--------------------------------------- Task 5 ----------------------------------------------------
-- Usage: > updateAll [] testData [22,22,22,22,22,22,22,22,22,22,22,22,22,22]

updateAll :: [Place] -> [Place] -> [Int] -> [Place]
updateAll newPlaces testData nums 
        | (length newPlaces) < 14 = updateAll (newPlaces ++ [(updateData (testData!!x) (nums!!x))]) testData nums
        | otherwise               = newPlaces
        where x = (length newPlaces)
		

updateData :: Place -> Int -> Place
updateData (place, north, east, d1, d2, d3, d4, d5, d6, d7) newVal = (place, north, east, newVal, d1, d2, d3, d4, d5, d6)



--------------------------------------- Task 6 -----------------------------------------------------

-- vi. replace a given existing place with a new place (the new place will have a name, a
-- location and a list of 7 rainfall figures)

-- Usage: > updatePlace [] testData [("Portsmouth" ,50.5,30.2,1,2,3,4,5,6,7)]

updatePlace :: [Place] -> [Place] -> [Place] -> [Place]
updatePlace newPlaces testData newRecord 
         | (length newPlaces) < 14 = updatePlace (newPlaces ++ [(updateRecord (testData!!x) (newRecord!!x))]) testData newRecord
         | otherwise               = newPlaces
         where x = (length newPlaces)


updateRecord :: Place -> Place -> Place
updateRecord (place, north, east, d1, d2, d3, d4, d5, d6, d7) newRecord = (place, north, east, d1, d2, d3, d4, d5, d6, d7)



--------------------------------------- Task 7 -----------------------------------------------------



