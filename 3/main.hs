import Data.Char (digitToInt)
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, eol)
import Safe (initSafe)

readBatteryBanks :: Parser [[Int]]
readBatteryBanks = endBy bank eol <* eof
  where
    bank = many (digitToInt <$> digitChar)

bankJoltRating :: [Int] -> Int
bankJoltRating bank =
  foldl' (\acc b -> acc * 10 + b) 0 bank

selectBattteries :: [Int] -> [Int]
selectBattteries bank =
  let maxJolt = maximum (initSafe bank)
      (_, rest) = break (== maxJolt) bank
      secondMaxJolt = maximum (drop 1 rest)
   in [maxJolt, secondMaxJolt]

solve :: [[Int]] -> Int
solve = sum . map (bankJoltRating . selectBattteries)

main :: IO ()
main = do
  (batteryBanksFile : _) <- getArgs
  batteryBanks <- parseFile readBatteryBanks batteryBanksFile

  let maxSumJoltRating = solve batteryBanks
  putStrLn $ "Max sum jolt rating: " <> show maxSumJoltRating
