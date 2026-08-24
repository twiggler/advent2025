module Number (assembleNumber) where

assembleNumber :: (Integral a) => [a] -> a
assembleNumber = foldl' (\acc b -> acc * 10 + b) 0
