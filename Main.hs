import System.IO
import Text.Read
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data MonthData = MonthData {year      :: Int,
                            month     :: Int,
                            tmax      :: Maybe Float,
                            tmin      :: Maybe Float,
                            frostDays :: Maybe Int,
                            rain      :: Maybe Float
                           } deriving (Show)

-- Takes a list of lines and turns them into months
parseData :: [String] -> [MonthData]
parseData = foldr parsingFunction [] . map words
            where parsingFunction line xs = MonthData{year      = read (line!!0),
                                                      month     = read (line!!1),
                                                      tmax      = readMaybe (line!!2),
                                                      tmin      = readMaybe (line!!3),
                                                      frostDays = readMaybe (line!!4),
                                                      rain      = readMaybe (line!!5)}:xs

extractFloat :: Maybe Float -> Float
extractFloat temp = case temp of Nothing -> 0.0
                                 Just x  -> x

mean :: (Fractional a) => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

main :: IO ()
main = do
  dataFile <- readFile "heathrow.txt"
  let weatherData = parseData $ drop 7 $ lines dataFile
--  mapM_ putStrLn $ map show weatherData

  toFile def "test.png" $ do
    layout_title .= "Test Graph"
    layout_x_axis . laxis_title .= "Temperature (degrees C)"
    layout_y_axis . laxis_title .= "Rainfall (cm)"
    setColors [opaque blue, opaque red, opaque cyan]
    let june  = [(extractFloat $ tmax m, extractFloat $ rain m) | m <- weatherData, month m == 6]
        july = [(extractFloat $ tmax m, extractFloat $ rain m) | m <- weatherData, month m == 7]
        august = [(extractFloat $ tmax m, extractFloat $ rain m) | m <- weatherData, month m == 8]
--    plot (points "june"  june)
--    plot (points "july" july)
    plot (points "august" august)
