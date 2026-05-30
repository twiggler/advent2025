import Control.Error.Util (note)
import Math.NumberTheory.Logarithms (integerLog10')
import Parsing (Parser, parseFile)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data ValidationRange = Empty | ValidationRange !Integer !Integer
  deriving (Show)

data AppError = InvalidInterval deriving (Show)

readProductIdRanges :: Parser [(Integer, Integer)]
readProductIdRanges = sepBy productIdRange (char ',') <* eol <* eof
  where
    productIdRange = (,) <$> L.decimal <* char '-' <*> L.decimal

nDigits :: Integer -> Int
nDigits 0 = 1
nDigits x = integerLog10' x + 1

minId, maxId :: Int -> Integer
minId n = 10 ^ (n - 1)
maxId n = 10 ^ n - 1

mkIdInterval :: (Integer, Integer) -> Maybe ValidationRange
mkIdInterval (a, b)
  | a > b = Nothing
  | a <= 0 = Nothing
  | digitsB - digitsA > 1 = Nothing
  | otherwise = case (even digitsA, even digitsB) of
      (True, True) -> Just $ ValidationRange a b
      (True, False) -> Just $ ValidationRange a (maxId digitsA)
      (False, True) -> Just $ ValidationRange (minId digitsB) b
      (False, False) -> Just Empty
  where
    digitsA = nDigits a
    digitsB = nDigits b

splitId :: Integer -> (Integer, Integer)
splitId a = a `quotRem` (10 ^ (nDigits a `div` 2))

doubleId :: Integer -> Integer
doubleId a = a + a * 10 ^ nDigits a

invalidIdsInRange :: ValidationRange -> [Integer]
invalidIdsInRange Empty = []
invalidIdsInRange (ValidationRange a b)
  | a2 == b2 = [doubleId b2 | a1 <= b2 && b2 <= b1]
  | otherwise = left ++ middle ++ right
  where
    (a2, a1) = splitId a
    (b2, b1) = splitId b
    left = [doubleId a2 | a1 <= a2]
    middle = [doubleId x | x <- [succ a2 .. pred b2]]
    right = [doubleId b2 | b2 <= b1]

solve1 :: [(Integer, Integer)] -> Either AppError Integer
solve1 ranges =
  note InvalidInterval (traverse mkIdInterval ranges)
    >>= Right . sum . concatMap invalidIdsInRange

main :: IO ()
main = do
  (productIdRangesFile : _) <- getArgs
  productIdRanges <- parseFile readProductIdRanges productIdRangesFile
  case solve1 productIdRanges of
    Left err -> putStrLn $ "Error: " ++ show err
    Right invalidIdSum -> putStrLn $ "Sum of invalid product IDs is: " ++ show invalidIdSum
