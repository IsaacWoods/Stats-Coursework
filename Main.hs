import System.IO
import Text.Read
import Data.List
import Data.Maybe
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

calcMean :: (Fractional a) => [a] -> a
calcMean xs = (sum xs) / (fromIntegral $ length xs)

calcStandardDeviation :: (Floating a, Fractional a) => [a] -> a
calcStandardDeviation xs =
  let mean = calcMean xs
  in sqrt $ (sum $ (map (^^2) $ map (subtract mean) xs))/(fromIntegral $ length xs)

elim :: Maybe Float -> Float
elim temp = case temp of Nothing -> 0.0
                         Just x  -> x

calculatePoints :: WeatherStation -> [(Float,Float)]
calculatePoints station =
  let june        = [(elim $ tmax m, elim $ rainfall m) | m<-(weatherData station), month m==6, isJust $ tmax m, isJust $ rainfall m]
      july        = [(elim $ tmax m, elim $ rainfall m) | m<-(weatherData station), month m==7, isJust $ tmax m, isJust $ rainfall m]
      august      = [(elim $ tmax m, elim $ rainfall m) | m<-(weatherData station), month m==8, isJust $ tmax m, isJust $ rainfall m]
      mean a b c  = (((fst a)+(fst b)+(fst c))/3, ((snd a)+(snd b)+(snd c))/3)
  in zipWith3 (mean) june july august

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort [l | l<-xs, l<=x])++[x]++(qsort [r | r<-xs, r>x])

rank :: (Ord a, Fractional a) => [a] -> [a]
rank xs = let n = fromIntegral $ length xs
              xs' = qsort xs
              rankerThingy x xs = let is = elemIndices x xs'
                                  in (realToFrac (foldl (+) 0 is)) / (fromIntegral $ length is)
          in map (\x -> n - ((rankerThingy x xs'))) xs

-- NOTE(Isaac): there are assumed to be as many items of data in both sets
calcSpearmans :: (Ord a, Fractional a) => [a] -> [a] -> a
calcSpearmans xs ys = let n = fromIntegral $ length xs
                  in 1.0-(6.0*(sum $ map (^^2) $ zipWith (-) (rank xs) (rank ys)))/(n * (n^^2-1.0))

main :: IO ()
main = do
  weatherStations <- mapM (readFile . ("stations/"++)) ["armagh.txt", "heathrow.txt", "lowestoft.txt", "rossonwye.txt", "sheffield.txt", "nairn.txt", "durham.txt"]
  let stationData = map calculatePoints $ map parseData weatherStations
      temps       = concat $ map (map fst) stationData
      rainfalls   = concat $ map (map snd) stationData
      spearmans   = calcSpearmans temps rainfalls

  putStrLn $ ("Spearman's Rank Correlation Coefficient: "++) $ show $ spearmans
  putStrLn $ show $ calcStandardDeviation [4.0,7.0,2.0,4.0,11.0]

  toFile def "test.png" $ do
    layout_y_axis . laxis_title .= "Rainfall (cm)"
    setColors [opaque blue, opaque red, opaque cyan, opaque darkred, opaque green, opaque dimgrey, opaque deeppink]
    layout_title .= "Maximum Temperature vs. Rainfall"
    layout_x_axis . laxis_title .= "Maximum Temperature (°C)"
    mapM_ (\station -> plot (points (name station) (calculatePoints station))) $ map parseData weatherStations
