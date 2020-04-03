-- UP918631

import Data.List
import System.IO
import Text.Printf

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
displayNames nameList = [p | (p, n, s, a, b, c, d, e, f, g) <- nameList ]



---------------------------------------- Task 2------------------------------------------------
-- Use: > getAverage testData "London"

getAverage :: [Place] -> String -> Float
getAverage [] _ = 0
getAverage (p:ps) find
    | find == name = avg [a,b,c,d,e,f,g]
    | otherwise = getAverage ps find
    where
        (name, _,_ , a,b,c,d,e,f,g) = p
        fi = fromIntegral 
        avg :: [Int] -> Float
        avg xs = (fi . sum $ xs) / (fi . length $ xs)


--------------------------------------- Task 3 -------------------------------------------------
-- Use: > putStrLn(placesToString testData)

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
                                                      | (location, _, _ , mon, tue, wed, thu, fri, sat, sun ) <- places ]

								
--------------------------------------- Task 4 ----------------------------------------------------

--getDryPlaces :: [Place] -> Int -> [String]
--getDryPlaces testData days = [location | (location, _, _, mon, tue, wed, thu, fri, sat, sun) <- testData ]

--return a list of the names of places that were totally dry (i.e. had zero rainfall) a given
--number of days ago (1 meaning yesterday, 7 meaning a week ago)

--days:
--1 = sun
--2 == sat
--3 = mon
--4 =  tue

getDay :: Int -> String
getDay x
  | x == 1 = "sun"
  | x == 2 = "sat"
  | x == 3 = "fri"
  | x == 4 = "thu"
  | x == 5 = "wed"
  | x == 6 = "tue"
  | x == 7 = "mon"
  

getDryDays :: [Place] -> Int -> [String]
getDryDays testData daysAgo = [ name | (name, _ , _, mon, tue, wed, thu, fri, sat, sun) <- testData, 
                                                                                                     daysAgo == 7 && sun == 0 || 
																									 daysAgo == 6 && sat == 0 ||
																									 daysAgo == 5 && fri == 0 ||
																									 daysAgo == 4 && thu == 0 ||
																									 daysAgo == 3 && wed == 0 ||
																									 daysAgo == 2 && tue == 0 ||
																									 daysAgo == 1 && mon == 0]

						


		

		
		
		
		

						  








