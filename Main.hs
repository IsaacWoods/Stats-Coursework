{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

import System.IO
import Text.Read
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data MonthData = MonthData {year      :: Int,
                            month     :: Int,
                            tmax      :: Maybe Float,
                            tmin      :: Maybe Float,
                            frostDays :: Maybe Int,
                            rainfall  :: Maybe Float
                           } deriving (Show)

data WeatherStation = WeatherStation {name :: String, weatherData :: [MonthData]}

-- Takes a list of lines and turns them into months
parseData :: String -> WeatherStation
parseData file = WeatherStation ((lines file) !! 0) (foldr parsingFunction [] $ map words $ drop 1 $ lines file)
                 where parsingFunction line xs = MonthData{year      = read (line!!0),
                                                           month     = read (line!!1),
                                                           tmax      = readMaybe (line!!2),
                                                           tmin      = readMaybe (line!!3),
                                                           frostDays = readMaybe (line!!4),
                                                           rainfall  = readMaybe (line!!5)}:xs

extractFloat :: Maybe Float -> Float
extractFloat temp = case temp of Nothing -> 0.0
                                 Just x  -> x

-- Takes an average of the minimum and maximum temperatures and plots that against rainfall
plotStation_MeanMinMax :: WeatherStation -> EC (Layout Float Float) ()
plotStation_MeanMinMax station = do
--  let weatherData = parseData $ drop 7 $ lines weatherFile
  let juneMin     = [(extractFloat $ tmin m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 6]
      julyMin     = [(extractFloat $ tmin m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 7]
      augustMin   = [(extractFloat $ tmin m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 8]
      juneMax     = [(extractFloat $ tmax m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 6]
      julyMax     = [(extractFloat $ tmax m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 7]
      augustMax   = [(extractFloat $ tmax m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 8]
      june        = zipWith (\a b -> (((fst a) + (fst b)) / 2, ((snd a) + (snd b)) / 2)) juneMin juneMax
      july        = zipWith (\a b -> (((fst a) + (fst b)) / 2, ((snd a) + (snd b)) / 2)) julyMin julyMax
      august      = zipWith (\a b -> (((fst a) + (fst b)) / 2, ((snd a) + (snd b)) / 2)) augustMin augustMax
      mean        = zipWith3 (\a b c -> (((fst a) + (fst b) + (fst c)) / 3, ((snd a) + (snd b) + (snd c)) / 3)) june july august
  plot (points (name station) mean)

-- Plots maximum temperature against rainfall
plotStation_Max :: WeatherStation -> EC (Layout Float Float) ()
plotStation_Max station = do
--  let weatherData = parseData $ drop 7 $ lines weatherFile
  let june        = [(extractFloat $ tmax m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 6]
      july        = [(extractFloat $ tmax m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 7]
      august      = [(extractFloat $ tmax m, extractFloat $ rainfall m) | m <- (weatherData station), month m == 8]
      mean        = zipWith3 (\a b c -> (((fst a) + (fst b) + (fst c)) / 3, ((snd a) + (snd b) + (snd c)) / 3)) june july august
  plot (points (name station) mean)

main :: IO ()
main = do
  weatherStations <- mapM (readFile . ("stations/"++)) ["armagh.txt", "heathrow.txt", "lowestoft.txt", "rossonwye.txt", "sheffield.txt", "nairn.txt", "eskdalemuir.txt"]

  toFile def "test.png" $ do
    layout_title .= "Test Graph"
    layout_x_axis . laxis_title .= "Average Temperature (°C)"
    layout_y_axis . laxis_title .= "Rainfall (cm)"
    setColors [opaque blue, opaque red, opaque cyan]
    mapM_ plotStation_Max $ map parseData weatherStations
