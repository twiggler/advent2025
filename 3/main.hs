import Data.Char (digitToInt)
import Data.Vector.Unboxed qualified as V
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, eol)

readBatteryBanks :: Parser [[Int]]
readBatteryBanks = endBy bank eol <* eof
  where
    bank = many (digitToInt <$> digitChar)

bankJoltRating :: [Int] -> Int
bankJoltRating bank =
  foldl' (\acc b -> acc * 10 + b) 0 bank

-- After reading other solutions, it turns out I implemented a monotonic stack, implemented using a vector.
-- The vector is probably faster for small n, but asymptotically a list approach would win.
selectBatteries :: Int -> [Int] -> [Int]
selectBatteries n bank =
    V.toList $ foldl' maximizeJolt initialBatteries withIndex
    where
        l = length bank
        initialBatteries = V.replicate n 0
        withIndex = zip [0 .. l - 1] bank

        maximizeJolt activeBatteries (candidateIndex, candidateJolt) =
            case V.findIndex isBetter (V.indexed activeBatteries) of
                Just i -> V.imap (swapBattery i) activeBatteries 
                Nothing -> activeBatteries
            where
                isBetter (activeIndex, activeJolt) =
                    candidateJolt > activeJolt && (l - candidateIndex) >= (n - activeIndex)    

                swapBattery targetIndex activeIndex activeJolt
                    | activeIndex < targetIndex = activeJolt
                    | activeIndex == targetIndex = candidateJolt
                    | otherwise = 0
           
solve :: Int -> [[Int]] -> Int
solve n = sum . fmap (bankJoltRating . (selectBatteries n))

main :: IO ()
main = do
  (batteryBanksFile : _) <- getArgs
  batteryBanks <- parseFile readBatteryBanks batteryBanksFile

  let maxSumJoltRating2 = solve 2 batteryBanks
  putStrLn $ "Max sum jolt rating for 2 batteries: " <> show maxSumJoltRating2

  let maxSumJoltRating12 = solve 12 batteryBanks
  putStrLn $ "Max sum jolt rating for 12 batteries: " <> show maxSumJoltRating12
