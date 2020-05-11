--
-- MATHFUN
-- UP918631
--

import Data.List
import Text.Printf
import System.Exit
import Control.Exception
import Text.Read

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
                    
--- Demo Functions

demo :: Int -> IO ()
demo 1 = mapM_ putStrLn $ displayNames testData
demo 2 = putStrLn $ printf "%.2f" $ returnAvgRain testData "Cardiff"
demo 3 = putStrLn(placesToString testData)
demo 4 = mapM_ putStrLn $ returnDryDays testData 2
demo 5 = print(updateRainfall testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0])
demo 6 = putStrLn $ show $ updateList testData ("Portsmouth", (50.8, -1.1), [0, 0, 3, 2, 5, 2, 1]) ("Plymouth", (50.4, -4.1), [4,9,0,0,0,6,5])
demo 7 = putStrLn $ show $ closestDry (50.9, -1.3) testData
demo 8 = plotMap(testData)
exit = exitApp(testData)

--
--- Numerical Functions 
-- Rounding to 2dp
truncate' :: Float -> Int -> Float
truncate' num decimalPlace = (fromIntegral (round (num * value))) / value
    where value = 10^decimalPlace 
    
-- Average of list
avg :: [Int] -> Float
avg rain = (fromIntegral . sum $ rain) / (fromIntegral . length $ rain) 


-- 
--- Task 1
-- Usage: > displayNames testData
displayNames :: [Place] -> [String]
displayNames places = [name | (name, _, _) <- places ]


--
--- Task 2
-- Usage: > returnAvgRain testData "Cardiff"
returnAvgRain :: [Place] -> String -> Float
returnAvgRain [] _ = 0
returnAvgRain (currentPlace:placeList) find
   | find == name = (avg rain)  -- call avg to calculate average rain as per place specified.
   | otherwise = returnAvgRain (placeList) find
      where (name, _, rain) = currentPlace
  
--
--- Task 3
-- Usage: > putStrLn(placesToString testData)
-- Creates padding between strings to make it level
createPadding char repeatChar inputString = inputString ++ replicate (repeatChar - length inputString) char 

placesToString :: [Place] -> String
placesToString places = unlines [intercalate "  |  " $ [createPadding ' ' 10 location] ++ map (createPadding ' ' 3 . show) rain | (location, _, rain) <- places ]


--
--- Task 4
-- Usage: > returnDryDays testData 2
returnDryDays :: [Place] -> Int -> [String]
returnDryDays places daysAgo = [name | (name, coords, rain) <- places, rain!!(daysAgo-1) == 0 ] -- index-1 that is equal to 0
 
 
-- 
--- Task 5
-- Usage: > updateRainfall testData [22,14,0,52,25,11,9,11,12,22,12,12,7,2]      
updateRainfall :: [Place] -> [Int] -> [Place]
updateRainfall places newRainfall = [(name,coords,newRain:init(previousRain))| ((name,coords,previousRain), newRain) <- joinPlaceAndRain] 
    where joinPlaceAndRain = zip places newRainfall -- zip <- make a list of tuples, elements occur at same position in list                 
     
     
--   
--- Task 6
-- Usage: > updateList testData ("Portsmouth", (50.8, -1.1), [0,0,3,2,5,2,1]) ("Plymouth", (50.4, -4.1), [4,9,0,0,0,6,5]))
updateList :: [Place] -> Place -> Place -> [Place]
updateList places newPlace oldPlace = map (updateRecord newPlace places oldPlace) places -- map returns a list by appling a function to all items in a list

updateRecord :: Place -> [Place] -> Place -> Place -> Place -- items in functional programming can not be "updated", must be re-created...
updateRecord newPlace places oldPlace(name, coords, [r1,r2,r3,r4,r5,r6,r7]) -- 'r' refers to rainfall
   | (name, coords, [r1,r2,r3,r4,r5,r6,r7]) == oldPlace = (newPlace)
   | otherwise = (name, coords, [r1,r2,r3,r4,r5,r6,r7])
  
  
--  
--- Task 7
-- Usage: > closestDry (54.6, -5.9) testData
returnDistance (north1 , east1) (north2 , east2) = sqrt $ (north2 - north1)^2 + (east2 - east1)^2 -- $ eliminate some parthensis, already readable

calculateDistance :: (Float, Float) -> Place -> Place -> Place
calculateDistance from toL1@(_,coordinates1,_) toL2@(_,coordinates2,_) -- @ means read as
   | returnDistance from coordinates1 > returnDistance from coordinates2 = toL2 | otherwise = toL1

closestDry :: (Float, Float) -> [Place] -> Place
closestDry coords testData = foldr1 (calculateDistance coords) [location | location@(name,_,rain) <- testData, (rain!!0 == 0)]


--
--- Rainfall Map
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
 
--Plot each place at corresponding coordinate
plotMap :: [Place] -> IO ()
plotMap places = do 
   clearScreen
   mapM_ (uncurry writeAt) (map (\(name, (north, east), rain) -> ((round ((east * 10) + 70), round (120 - (north * 2))), "+" ++ name ++ " " ++ show (truncate' (avg rain) 2))) places)
   goTo(0,80)
 

--
--- UI 
-- Usage: > main
main = do
    fileData <- readFile "places.txt"
    putStrLn "\nLocations: \n"
    mapM_ putStrLn $ displayNames testData
    menu (read fileData)
     
menu :: [Place] -> IO ()
menu fileData = do
     putStrLn "\nPick an option from below: \n"
     putStrLn . unlines $ map concatNums (choices fileData)
     choice <- getLine
      
     case validateOpt fileData choice of
        Just n  -> execute fileData . read $ choice 
        Nothing -> do 
           putStrLn "Please enter a valid input..."
           menu(fileData)

      where concatNums (i, (s, _)) = show i ++ ".) " ++ s

validateOpt :: [Place] ->  String -> Maybe Int
validateOpt fileData s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
            | outOfBounds n    = Nothing
            | otherwise        = Just n
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
   ("Display map", choiceMap fileData),
   ("Exit program and save data", exitApp fileData)
 ]


choice1(fileData) = do     
         mapM_ putStrLn $ displayNames testData
         menu fileData
                                 
choice2(fileData) = do
         putStrLn "Enter a city name i.e. London: "
         city <- getLine
         result <- try (putStrLn $ printf "%.2f" $ returnAvgRain fileData (city)) :: IO (Either SomeException ()) 
         case result of 
            Left _  -> do
                        putStrLn "Please enter an existing city..."
                        choice2 fileData 
            Right _ -> do
                        menu fileData
                                
choice3(fileData) = do
         putStrLn $ placesToString fileData
         menu fileData
                                
choice4(fileData) = do            
         putStrLn "Enter no. of days ago: "
         days <- getLine
         result <- try ( mapM_ putStrLn $ (returnDryDays fileData (read days))) :: IO (Either SomeException ())
         case result of
            Left _  -> do
                        putStrLn "Enter a valid input...(1-7): "
                        choice4 fileData 
            Right _ -> do
            
                        menu fileData                                     
                           
choice5(fileData) = do
         putStrLn "Enter new 14 rainfall figures i.e [1,..,14]: "
         newData <- getLine
         
         case (readMaybe newData :: Maybe [Int]) of
              Nothing -> do
                    putStrLn "Invalid Input, should follow format [1,..,14]: "
                    choice5 fileData 
              Just n -> do
                    let result = length n
                    case (result == length(fileData)) of
                      True -> do
                        menu (updateRainfall fileData (read newData)) 
                      False -> do
                        putStrLn "Invalid Input, should follow format [1,..,14]: "
                        choice5 fileData

choice6(fileData) = do
         putStrLn "Enter (name, (n,e), [rainfall]) data for your new location: "    
         inputOld <- getLine
         let newPlace = (read inputOld :: Place)

         putStrLn "Enter (name, (n,e), [rainfall]) data for your old location: "
         inputNew <- getLine
         let oldPlace = (read inputNew :: Place)

         result <- try (putStrLn $ show $ updateList fileData newPlace oldPlace) :: IO (Either SomeException ())
         case result of
            Left _  -> do
                        putStrLn "Format entered is incorrect..."
                        putStrLn "Should follow (name, (n,e), []) with parenthesis around the name."
                        choice6(fileData)
            Right _ -> do
                        menu(updateList fileData newPlace oldPlace)
 
choice7(fileData) = do
         putStrLn "Enter coordinates to find closest dry place i.e. (54.6, -5.9): "
         input <- getLine
         let coords = (read input :: (Float,Float))
         result <- try (putStrLn $ show $ closestDry coords fileData) :: IO (Either SomeException ())
         case result of
            Left _  -> do
                        choice7 fileData 
            Right _ -> do
                        menu fileData

choiceMap(fileData) = do
         plotMap(fileData)
         menu(fileData)

exitApp(fileData) = do 
         result <- try (writeFile "places.txt" (show fileData)) :: IO (Either SomeException ())
         case result of
            Left _  -> do
                        putStrLn "Unable to save file, try again...:"
                        menu fileData 
            Right _ -> do
                        exitSuccess

                                

execute :: [Place] -> Int -> IO ()
execute fileData n = doExec $ filter (\(i, _) -> i == n) (choices fileData)    
   where doExec ((_, (_,f)):_) = f




