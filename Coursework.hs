--
-- MATHFUN
-- UP918631
--

import Data.List
import System.IO
import Control.Monad
import System.Exit
import Control.Exception
import System.Timeout
import Control.Concurrent


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
demo 8 = plotMap(testData)
exit = exitApp(testData)

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

-- Usage: > updateList testData ("Portsmouth", (50.8, -1.1), [0,0,3,2,5,2,1]) ("Plymouth", (50.4, -4.1), [4,9,0,0,0,6,5]))


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

-- Usage: > plotMap(testData)

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
    
plotMap :: [Place] -> IO ()
plotMap places = do 
   clearScreen
   mapM_ (uncurry writeAt) (map (\(x, (n1, n2), z) -> ((round ((n2 * 10) + 70), round (120 - (n1 * 2))), "+" ++ x ++ " " ++ show (truncate' (avg z) 2))) places)
   goTo(0,80)
 

--------------------------------------------- UI ---------------------------------------------------
main = do
     fileData <- readFile "places.txt"
     print (displayNames (read fileData))
     menu (read fileData)

menu :: [Place] -> IO ()
menu fileData = do
      putStrLn . unlines $ map concatNums (choices fileData)
      choice <- getLine
      case validate fileData choice of
         Just n  -> execute fileData . read $ choice 
         Nothing -> do 
                     putStrLn "Please enter a valid input..."
                     main

   where concatNums (i, (s, _)) = show i ++ ".) " ++ s

validate :: [Place] ->  String -> Maybe Int
validate fileData s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 1) || (n > length (choices fileData) )

choices :: [Place] -> [(Int, (String, IO ()))]
choices fileData = zip [1.. ] [
   ("Display a list of all locations", choice1 fileData),
   ("Display average rainfall for a given location", choice2 fileData),
   ("Display all places and rainfall Figures", choice3 fileData),
   ("Display all places dry days ago", choice4 fileData),
   ("Update rainfall figures", choice5 fileData),
   ("Replace an existing place", choice6 fileData),
   ("Show the closest dry place from a given point", choice7 fileData),
   ("Display map", plotMap fileData),
   ("Exit program and save data", exitApp fileData)
 ]


choice1(fileData) = do     
                 print(displayNames fileData) 
                 menu(fileData)
                                 
choice2(fileData) = do
                 putStrLn("Enter a city name i.e. London: ")
                 city <- getLine
                 result <- try (print(returnAverage fileData (read city))) :: IO (Either SomeException ())
                 case result of
                    Left _  -> do
                                putStrLn "Please enter an existing city with quotation marks"
                                choice2(fileData)
                    Right _ -> do
                                menu(fileData) 
                                
choice3(fileData) = do
                 putStrLn(placesToString fileData)
                 menu(fileData)
                                
choice4(fileData) = do            
                 putStrLn("Enter no. of days ago: ")
                 days <- getLine
                 result <- try (print(dryDays fileData (read days))) :: IO (Either SomeException ())
                 case result of
                    Left _  -> do
                                choice4(fileData)
                    Right _ -> do
                                menu(fileData)                                     
            
choice5(fileData) = do
                 putStrLn("Enter new 14 rainfall figures i.e [1,..,14]: ")
                 newData <- getLine
                 result <- try (print(updateAllRainfall fileData (read newData))) :: IO (Either SomeException ())
                 case result of
                    Left _  -> do
                                choice5(fileData)
                    Right _ -> do
                                menu(updateAllRainfall fileData (read newData)) 
   
choice6(fileData) = do
                 putStrLn("Enter Place data for your new location: ")     
                 inputOld <- getLine
                 let newPlace = (read inputOld :: Place)

                 putStrLn("Enter Place data for your old location: ")
                 inputNew <- getLine
                 let oldPlace = (read inputNew :: Place)

                 result <- try (print(updateList fileData newPlace oldPlace)) :: IO (Either SomeException ())
                 case result of
                    Left _  -> do
                                putStrLn "Format entered is incorrect..."
                                putStrLn "Should follow (name, (n,e), []) with parenthesis around the name."
                                choice6(fileData)
                    Right _ -> do
                                menu(updateList fileData newPlace oldPlace)
 
choice7(fileData) = do
                 putStrLn("Enter coordinates to find closest dry place i.e. (54.6, -5.9): ")
                 input <- getLine
                 let coordinates = (read input :: (Float,Float))
                 result <- try (print(closestDry coordinates fileData)) :: IO (Either SomeException ())
                 case result of
                    Left _  -> do
                                choice7(fileData)
                    Right _ -> do
                                menu(fileData) 

choiceMap(fileData) = do
                 plotMap(fileData)
                 menu(fileData)

exitApp(fileData) = do 
                 result <- try (writeFile "places.txt" (show fileData)) :: IO (Either SomeException ())
                 case result of
                    Left _  -> do
                                putStrLn("Unable to save file, try again...:")
                                menu(fileData) 
                    Right _ -> do
                                exitSuccess

execute :: [Place] -> Int -> IO ()
execute fileData n = doExec $ filter (\(i, _) -> i == n) (choices fileData)    
   where doExec ((_, (_,f)):_) = f




