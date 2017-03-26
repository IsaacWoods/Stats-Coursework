{-# LANGUAGE PatternGuards, TemplateHaskell, FlexibleInstances #-}

import System.IO
import Text.Read
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad (when)
import Control.Monad.ST
import Control.Lens
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Matrix as M
import qualified Graphics.Rendering.Chart.Easy as C
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
elim Nothing = 0.0
elim (Just x) = x

calculateAllPoints :: WeatherStation -> [(Float,Float)]
calculateAllPoints station =
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

extractRight :: Either a b -> b
extractRight (Right x) = x
extractRight (Left _) = error "Failed to find inverse of matrix during least-squares regression analysis"

type LineEquation = (Float,Float)

calcRegressionLine :: [(Float,Float)] -> LineEquation
calcRegressionLine points = let n             = fromIntegral $ length points
                                sumXn         = sum $ map (fst) points
                                sumXnSquared  = sum $ map (^^2) $ map (fst) points
                                sumXnYn       = sum $ zipWith (*) (map (fst) points) (map (snd) points)
                                sumYn         = sum $ map (snd) points
                                m             = fromList 2 2 [sumXnSquared,sumXn,sumXn,n]
                                vector        = fromList 2 1 [sumXnYn,sumYn]
                                solution      = (extractRight $ inverse m) * vector
                            in (solution!(1,1), solution!(2,1))

plotPointsForLine :: LineEquation -> Float -> Float -> [(Float,Float)]
plotPointsForLine equation xStart xEnd = (plotPoint equation xStart):[(plotPoint equation xEnd)]
                                         where plotPoint equation x = (x, x*(fst equation) + (snd equation))

extractDecadeTemps :: Int -> WeatherStation -> [Float]
extractDecadeTemps decade station =
  let monthPred m = (month m) `elem` [6,7,8]
      decadePred m = (year m) `elem` [decade, (decade+1) .. (decade + 9)]
  in [elim $ tmax m | m <- (weatherData station), monthPred m, decadePred m, isJust $ tmax m]

type Range a = (a,a)

histWithBins :: (RealFrac a) => V.Vector (Range a) -> [a] -> V.Vector (Range a, a)
histWithBins bins xs =
    let n = V.length bins
        testBin :: RealFrac a => a -> (Int, Range a) -> Bool
        testBin x (i, (a,b)) =
            if i == n - 1
                then x >= a && x <= b
                else x >= a && x < b

        f :: (RealFrac a) => V.Vector (Range a) -> MV.STVector s a -> a -> ST s ()
        f bins1 bs x =
            case V.dropWhile (not . testBin x) $ V.indexed bins1 of
                v | V.null v  -> return ()
                v | (idx,_) <- V.head v  -> do
                    m <- MV.read bs idx
                    MV.write bs idx $! (m+1)

        counts = runST $ do b <- MV.replicate n 0
                            mapM_ (f bins b) xs
                            V.freeze b
    in V.zipWith (\bin count -> (bin, realToFrac $ count / ((snd bin)-(fst bin)))) bins counts

data PlotHist x = PlotHist
    {
      _plot_hist_title                :: String
    , _plot_hist_values               :: [x]
    , _plot_hist_no_zeros             :: Bool
    , _plot_hist_range                :: Maybe (x,x)
    , _plot_hist_drop_lines           :: Bool
    , _plot_hist_fill_style           :: C.FillStyle
    , _plot_hist_line_style           :: C.LineStyle
    }

histToPlot :: (RealFrac x) => PlotHist x -> [(x,x)] -> C.Plot x x
histToPlot p bins = C.Plot  { C._plot_render      = renderPlotHist p bins,
                              C._plot_legend      = [(_plot_hist_title p, renderPlotLegendHist p)],
                              C._plot_all_points  = unzip
                                                          $ concatMap (\((x1,x2), y)->[ (x1,y)
                                                                                      , (x2,y)
                                                                                      , (x1,0)
                                                                                      , (x2,0)
                                                                                      ])
                                                          $ histToBins p bins
    }

buildHistPath :: (RealFrac x) => C.PointMapFn x x -> [((x,x), x)] -> C.Path
buildHistPath _ [] = C.End
buildHistPath pmap bins = C.MoveTo (pt xb 0) (go bins)
    where go [((x1,x2),y)]      = C.LineTo (pt x1 y)
                                $ C.LineTo (pt x2 y)
                                $ C.LineTo (pt x2 0)
                                $ C.End
          go (((x1,x2),y):rest) = C.LineTo (pt x1 y)
                                $ C.LineTo (pt x2 y)
                                $ go rest
          go []                 = C.End
          ((xb,_),_) = head bins
          pt x y = pmap (C.LValue x, C.LValue y)

renderPlotHist :: (RealFrac x) => PlotHist x -> [(x,x)] -> C.PointMapFn x x -> C.BackendProgram ()
renderPlotHist p binBounds pmap
    | null bins = return ()
    | otherwise = do
        C.withFillStyle (_plot_hist_fill_style p) $ C.alignFillPath (buildHistPath pmap bins) >>= C.fillPath
        C.withLineStyle (_plot_hist_line_style p) $ do
            when (_plot_hist_drop_lines p) $ C.alignStrokePath dropLinesPath >>= C.strokePath
            C.alignStrokePath (buildHistPath pmap bins) >>= C.strokePath
    where bins = histToBins p binBounds
          pt x y = pmap (C.LValue x, C.LValue y)
          dropLinesPath = F.foldMap (\((x1,_), y)->C.moveTo (pt x1 0)
                                                <> C.lineTo (pt x1 y)
                                    ) $ tail bins

renderPlotLegendHist :: PlotHist x -> C.Rect -> C.BackendProgram ()
renderPlotLegendHist p (C.Rect p1 p2) =
    C.withLineStyle (_plot_hist_line_style p) $
        let y = (C.p_y p1 + C.p_y p2) / 2
        in C.strokePath $ C.moveTo' (C.p_x p1) y <> C.lineTo' (C.p_x p2) y

histToBins :: (RealFrac x) => PlotHist x -> [(x,x)] -> [((x,x), x)]
histToBins hist bounds =
    zip bounds counts
      where (a,b) = realHistRange hist
            dx = realToFrac (b-a) / (realToFrac $ length bounds)
            values = V.fromList (_plot_hist_values hist)
            counts = V.toList $ V.map (snd) $ histWithBins (V.fromList bounds) $ V.toList values

realHistRange :: (RealFrac x) => PlotHist x -> (x,x)
realHistRange hist = fromMaybe range $ _plot_hist_range hist
    where values = V.fromList (_plot_hist_values hist)
          range = if V.null values
                    then (0,0)
                    else (V.minimum values, V.maximum values)

$( makeLenses ''PlotHist )

createHistogram :: Int -> [(Float,Float)] -> [Float] -> IO (C.PickFn ())
createHistogram decade bins temps = renderableToFile C.def ("histogram_"++(show decade)++".png") $ C.toRenderable layout
  where histPlot = PlotHist { _plot_hist_title      = "",
                              _plot_hist_values     = temps,
                              _plot_hist_no_zeros   = True,
                              _plot_hist_range      = Just ((fromIntegral $ floor $ minimum temps), (fromIntegral $ ceiling $ maximum temps)),
                              _plot_hist_drop_lines = True,
                              _plot_hist_line_style = (C.solidLine 1 $ C.opaque C.black) { C._line_cap = C.LineCapButt, C._line_join = C.LineJoinMiter },
                              _plot_hist_fill_style = (C.solidFillStyle $ C.opaque C.blue)
                            }
        layout  = C.layout_title .~ (show decade)
                $ C.layout_x_axis . C.laxis_title .~ "Maximum Temperature (°C)"
                $ C.layout_y_axis . C.laxis_title .~ "Frequency Density"
                $ C.layout_plots .~ [histToPlot histPlot bins]
                $ C.def

main = do
  weatherStations <- mapM (readFile . ("stations/"++)) ["armagh.txt", "heathrow.txt", "lowestoft.txt", "rossonwye.txt", "sheffield.txt", "nairn.txt", "durham.txt"]
  let stationData = map (parseData) weatherStations
      allPoints   = map (calculateAllPoints) stationData
      temps       = concat $ map (map fst) allPoints
      rainfalls   = concat $ map (map snd) allPoints
      spearmans   = calcSpearmans temps rainfalls
      tempSD      = calcStandardDeviation temps
      rainfallSD  = calcStandardDeviation rainfalls
      plotPoints  = zip temps rainfalls
      bestFitLine = calcRegressionLine plotPoints
      tempRange   = (minimum temps,maximum temps)

  putStrLn $ ("Number of points plotted: "++) $ show $ length plotPoints
  putStrLn $ ("Spearman's Rank Correlation Coefficient: "++) $ show spearmans
  putStrLn $ ("Standard deviation of max temperature: "++) $ show tempSD
  putStrLn $ ("Standard deviation of rainfall: "++) $ show rainfallSD
  putStrLn $ ("Equation of line of best fit: "++) $ show bestFitLine
  putStrLn $ ("Range of temperatures: "++) $ show tempRange
  
  -- Plot the scatter graph
  toFile C.def "scatter.png" $ do
    C.layout_y_axis . C.laxis_title .= "Rainfall (cm)"
    C.setColors [C.opaque C.blue, C.opaque C.red, C.opaque C.cyan, C.opaque C.darkred, C.opaque C.green, C.opaque C.dimgrey, C.opaque C.deeppink, C.opaque C.black]
    C.layout_title .= "Maximum Temperature vs. Rainfall"
    C.layout_x_axis . C.laxis_title .= "Maximum Temperature (°C)"
    mapM_ (\station -> C.plot (C.points (name station) (calculateAllPoints station))) stationData
    -- The values chosen for the x-range are hacky and horrible but cba to do it properly
    C.plot (C.line "Line of Best Fit" [(plotPointsForLine bestFitLine (fst tempRange) (snd tempRange))])

  -- Plot the histograms
  mapM (\decade -> createHistogram decade bins $ concat $ map (extractDecadeTemps decade) stationData) [1940,1950..2000]
    where bins = [(15.0,17.0),(17.0,19.0),(19.0,20.0),(20.0,21.0),(21.0,22.0),(22.0,24.0),(24.0,26.0)]
