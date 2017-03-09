import System.IO
import Text.Read
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

signal :: [Double] -> [(Double,Double)]
signal xs = [(x,(sin (2*x))) | x <- xs]

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

main :: IO ()
main = do
  dataFile <- readFile "heathrow.txt"
  mapM_ putStrLn $ map show $ parseData $ drop 7 $ lines dataFile
  toFile def "test.png" $ do
    layout_title .= "Test Graph"
    setColors [opaque blue, opaque red]
    plot (line "am" [signal [0,(0.005)..(2*3.14)]])
