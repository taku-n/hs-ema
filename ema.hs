{-# OPTIONS -Wall #-}

ema :: [Double] -> Int -> [Double]
ema xs n =

nextEMA :: Double -> Double -> Int -> Double
nextEMA y' s n = a * y' + (1 - a) * s
    where a = 2.0 / (fromIntegral n + 1.0)
