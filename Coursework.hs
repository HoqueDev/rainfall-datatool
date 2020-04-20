--
-- MATHFUN
-- UP918631
--

import Data.List
import System.IO
import Control.Monad
import System.Exit

--
-- Types (define Place type here)
--

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
            
            
--------------------------------------------- Demo --------------------------------------------

demo :: Int -> IO ()
demo 1 = print(displayNames testData) 
demo 2 = print(returnAverage testData "Cardiff")
demo 3 = putStrLn (placesToString testData)
demo 4 = print(dryDays testData 2)
demo 5 = print(updateAllRainfall testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0])
demo 6 = print(updateList testData ("Portsmouth", (50.8, -1.1), [0, 0, 3, 2, 5, 2, 1]) ("Plymouth", (50.4, -4.1), [4,9,0,0,0,6,5]))
demo 7 = print(closestDry (50.9, -1.3) testData)
demo 8 = plotMap
exit = exitApp

------------------------------------ Numerical Functions --------------------------------------

--Rounding to 2dp
truncate' :: Float -> Int -> Float
truncate' num decimalPlace = (fromIntegral (floor (num * t))) / t
    where t = 10^decimalPlace
    
--Average
avg :: [Int] -> Float
avg rain = (fromIntegral . sum $ rain) / (fromIntegral . length $ rain)


----------------------------------------- Task 1 -----------------------------------------------

-- Use: > displayNames testData


displayNames :: [Place] -> [String]
displayNames testData = [name | (name, _, _) <- testData ]


----------------------------------------- Task 2------------------------------------------------

-- Use: > returnAverage testData "London"


returnAverage :: [Place] -> String -> Float
returnAverage [] _ = 0
returnAverage (p:ps) find
    | find == name = (truncate' (avg rainfall) 2)
    | otherwise = returnAverage (ps) find
    where (name, _, rainfall) = p
       

----------------------------------------- Task 3 -----------------------------------------------

-- -- Usage: > putStrLn(placesToString testData)


createPadding c n s = s ++ replicate (n - length s) c

placesToString :: [Place] -> String
placesToString places = unlines [intercalate "  |  " $ [createPadding ' ' 10 location] ++ map (createPadding ' ' 3 . show) rain | (location, _, rain) <- places ]

--------------------------------------- Task 4 ----------------------------------------------------

-- Usage: > returnDryDays testData 2

  --Task 4
dryDays :: [Place] -> Int -> [String]
dryDays places daysAgo = [place | (place, coords, rainfallData) <- places, rainfallData!!(daysAgo-1) == 0 ]
                  
                                 
--------------------------------------- Task 5 ----------------------------------------------------

-- Usage: > updateAllRainfall testData [22,14,0,52,25,11,9,11,12,22,12,12,7,2]

         
updateAllRainfall :: [Place] -> [Int] -> [Place]
updateAllRainfall placeList newRainfall = [(name,coords,newRain:init(previousRain))| ((name,coords,previousRain), newRain) <- zippedPlaceAndRain] where
    zippedPlaceAndRain = zip placeList newRainfall                  


--------------------------------------- Task 6 -----------------------------------------------------

-- Usage: > updateList testData ("Portsmouth", (51.5, -0.1), [12, 24, 36, 48, 55, 66, 70]) ("Lerwick", (60.2, -1.1), [8, 10, 5, 5, 0, 0, 3])


updateList :: [Place] -> Place -> Place -> [Place]
updateList places newPlace oldPlace = map (updateRecord newPlace places oldPlace) places 

updateRecord :: Place -> [Place] -> Place -> Place -> Place
updateRecord newPlace places oldPlace(name, coordinates, [a,b,c,d,e,f,g]) 
   | (name, coordinates, [a,b,c,d,e,f,g]) == oldPlace = (newPlace)
   | otherwise = (name, coordinates, [a,b,c,d,e,f,g])
                                                                    
                    
--------------------------------------- Task 7 -----------------------------------------------------

-- Usage: > closestDry (54.6, -5.9) testData

returnDistance (x1 , y1) (x2 , y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

calculateDistance :: (Float, Float) -> Place -> Place -> Place
calculateDistance from toL1@(_,coordinates1,_) toL2@(_,coordinates2,_)
    | returnDistance from coordinates1 > returnDistance from coordinates2 = toL2
    | otherwise = toL1

closestDry :: (Float, Float) -> [Place] -> Place
closestDry coordinates testData = foldr1 (calculateDistance coordinates) [location | location@(_,_,rain) <- testData, (rain!!0 == 0)]



------------------------------------- Rainfall Map -------------------------------------------------

-- Usage: > plotMap

type ScreenPosition = (Integer ,Integer)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text 
    
plotMap :: IO ()
plotMap = do 
   clearScreen
   mapM_ (uncurry writeAt) (map (\(x, (n1, n2), z) -> ((round ((n2 * 10) + 70), round (120 - (n1 * 2))), "+" ++ x ++ " " ++ show (truncate' (avg z) 2))) testData)
   goTo(0,80)
 

--------------------------------------------- UI ---------------------------------------------------

main :: IO ()
main = do
      testData <- openFile  "places.txt" ReadWriteMode
      hClose testData
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> execute . read $ choice
         Nothing -> putStrLn "Please enter a valid input"

      main
   where concatNums (i, (s, _)) = show i ++ ".) " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length choices)

choices :: [(Int, (String, IO ()))]
choices = zip [1.. ] [
   ("Display a list of all locations", (choice1)),
   ("Display average rainfall for a given location", choice2),
   ("Display all places and rainfall Figures", choice3),
   ("Display all places dry days ago", choice4),
   ("Update rainfall figures", choice5),
   ("Replace an existing place", choice6),
   ("Show the closest dry place from a given point", choice7),
   ("Display map", plotMap),
   ("Exit program and save data", exitApp)
 ]


choice1 = do     
                 print(displayNames testData) 
choice2 = do
                 putStrLn("Enter a city name i.e. London: ")
                 city <- getLine
                 
                 --Error Handling
                 case elem city (displayNames testData) of
                   True -> do
                    print(returnAverage testData city)
                   False -> do
                    putStrLn "Please enter an existing city"
                    choice2

choice3 = do
                 putStrLn(placesToString testData)
choice4 = do
                 putStrLn("Enter days ago: ")
                 input <- getLine
                 let daysAgo = (read input :: Int)
                 
                 case daysAgo >= 7 of
                   True -> do
                    print(dryDays testData daysAgo)
                   False -> do
                    putStrLn "Please enter a number from (1-7):"
                    choice4
            
choice5 = do
                 putStrLn("Enter new 14 rainfall figures i.e [1,..,14]")
                 input <- getLine
                 let newData = (read input :: [Int])
                 print(updateAllRainfall testData newData)  
choice6 = do
                 putStrLn("Enter place data for the NEW Place data i.e. (name, (n,e), [1,2,3,4,5,6,7])")     
                 input <- getLine
                 let newPlace = (read input :: Place)
                 case (read input :: Place) of
                  True -> do 
                   putStrLn "True"
                  False -> do
                    putStrLn "False:"

                 putStrLn("Enter place data for the OLD Place data i.e. (name, (n,e), [1,2,3,4,5,6,7])")
                 input <- getLine
                 let oldPlace = (read input :: Place)
                 
                 
                 print(updateList testData newPlace oldPlace)

                 
choice7 = do
                 putStrLn("Enter coordinates to find closest dry place i.e. (54.6, -5.9)")
                 input <- getLine
                 let coordinates = (read input :: (Float,Float))
                 print(closestDry coordinates testData)
exitApp = do 
                 writeFile "places.txt" (show testData)
                 return ()

  
execute :: Int -> IO ()
execute n = doExec $ filter (\(i, _) -> i == n) choices
   where doExec ((_, (_,f)):_) = f




