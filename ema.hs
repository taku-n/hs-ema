{-# OPTIONS -Wall #-}

ema :: [Double] -> Int -> [Double]
ema (s:ys) n = reverse $ foldl getNextEMA [s] ys
    where getNextEMA all@(s:_) y' = (a * y' + (1 - a) * s):all
          a                       = 2.0 / (fromIntegral n + 1.0)
