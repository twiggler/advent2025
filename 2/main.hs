import Data.Containers.ListUtils (nubOrd)
import Data.List (intersect)
import Data.List.NonEmpty qualified as NE
import Math.NumberTheory.Logarithms (integerLog10')
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data IdRange = Empty | IdRange !Int !Int
  deriving (Show)

readProductIdRanges :: Parser [(Int, Int)]
readProductIdRanges = sepBy productIdRange (char ',') <* eol <* eof
  where
    productIdRange = (,) <$> L.decimal <* char '-' <*> L.decimal

nDigits :: Int -> Int
nDigits 0 = 1
nDigits x = integerLog10' (toInteger x) + 1

idLength :: IdRange -> Int
idLength Empty = 0
idLength (IdRange low _) = nDigits low

primeFactors :: Int -> [Int]
primeFactors n = NE.head <$> NE.group (factor n 2)
  where
    factor 1 _ = []
    factor m f
      | f * f > m = [m]
      | m `mod` f == 0 = f : factor (m `div` f) f
      | otherwise = factor m (f + 1)

mkIdRanges :: (Int, Int) -> Maybe [IdRange]
mkIdRanges (a, b)
  | a > b = Nothing
  | a <= 0 = Nothing
  | digitsB - digitsA > 1 = Nothing
  | otherwise =
      Just $
        if digitsA == digitsB
          then [IdRange a b]
          else
            [ IdRange a (10 ^ digitsA - 1),
              IdRange (10 ^ (digitsB - 1)) b
            ]
  where
    digitsA = nDigits a
    digitsB = nDigits b

splitId :: Int -> Int -> (Int, Int)
splitId factor a = a `quotRem` (10 ^ (nDigits a - nDigits a `div` factor))

expandId :: Int -> Int -> Int
expandId n a = sum [a * 10 ^ (i * nDigits a) | i <- [0 .. n - 1]]

invalidIdsInRange :: IdRange -> Int -> [Int]
invalidIdsInRange Empty _ = []
invalidIdsInRange (IdRange low high) factor
  | lowerMajor == upperMajor = expandId factor <$> (left `intersect` right)
  | otherwise = expandId factor <$> (left ++ middle ++ right)
  where
    (lowerMajor, lowerMinor) = splitId factor low
    (upperMajor, upperMinor) = splitId factor high
    left = [lowerMajor | lowerMinor <= expandId (factor - 1) lowerMajor]
    middle = [succ lowerMajor .. pred upperMajor]
    right = [upperMajor | expandId (factor - 1) upperMajor <= upperMinor]

method1, method2 :: IdRange -> [Int]
method1 r = if even (idLength r) then invalidIdsInRange r 2 else []
-- nubOrd is needed because different prime factors can result in invalid ids consisting of a repeating digit.
method2 r = nubOrd $ invalidIdsInRange r `concatMap` primeFactors (idLength r)

solve :: (IdRange -> [Int]) -> [(Int, Int)] -> Maybe Int
solve method =
  fmap (sum . concatMap method . concat) . traverse mkIdRanges

main :: IO ()
main = do
  (productIdRangesFile : _) <- getArgs
  productIdRanges <- parseFile readProductIdRanges productIdRangesFile

  case solve method1 productIdRanges of
    Nothing -> putStrLn "Error: invalid validation range"
    Just sumMethod1 -> putStrLn $ "Sum of invalid Ids (method 1): " ++ show sumMethod1

  case solve method2 productIdRanges of
    Nothing -> putStrLn "Error: invalid validation range"
    Just sumMethod2 -> putStrLn $ "Sum of invalid Ids (method 2): " ++ show sumMethod2
