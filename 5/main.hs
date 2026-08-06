{-# LANGUAGE MultiParamTypeClasses #-}

import Data.IntervalSet (Interval (..))
import Data.IntervalSet qualified as IS
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data IngredientDatabase = IngredientDatabase
  { freshRanges :: [(Int, Int)],
    ingredients :: [Int]
  }
  deriving (Show)

data FreshIdRange = FreshIdRange Int Int deriving (Eq, Ord)

instance Interval FreshIdRange Int where
  lowerBound (FreshIdRange a _) = a
  upperBound (FreshIdRange _ b) = b

type FreshIdRangeSet = IS.IntervalSet FreshIdRange

rangeLength :: FreshIdRange -> Int
rangeLength (FreshIdRange a b) = b - a + 1

readIngredientDatabase :: Parser IngredientDatabase
readIngredientDatabase =
  IngredientDatabase <$> readFreshRanges <* eol <*> readIngredients <* eof
  where
    readFreshRanges = endBy freshRange eol
    readIngredients = endBy L.decimal eol
    freshRange = (,) <$> L.decimal <* char '-' <*> L.decimal

mkFreshIngredientSet :: [(Int, Int)] -> FreshIdRangeSet
mkFreshIngredientSet ranges = IS.fromList $ uncurry FreshIdRange <$> ranges

solve1 :: IngredientDatabase -> Int
solve1 (IngredientDatabase ranges ids) =
  let freshSet = mkFreshIngredientSet ranges
   in length $ filter (not . null . IS.containing freshSet) ids

solve2 :: IngredientDatabase -> Int
solve2 (IngredientDatabase ranges _) =
  let freshSet = mkFreshIngredientSet ranges
      flattenedFreshSet = IS.flattenWithMonotonic combine freshSet
   in sum $ rangeLength <$> IS.toList flattenedFreshSet
  where
    combine :: FreshIdRange -> FreshIdRange -> Maybe FreshIdRange
    combine r1 r2 =
      if IS.overlaps r1 r2
        then Just (FreshIdRange (IS.lowerBound r1) (max (IS.upperBound r1) (IS.upperBound r2)))
        else Nothing

main :: IO ()
main = do
  (ingredientDatabaseFile : _) <- getArgs
  ingredientDatabase <- parseFile readIngredientDatabase ingredientDatabaseFile

  let availableFreshCount = solve1 ingredientDatabase
  putStrLn $ "Number of fresh ingredients: " <> show availableFreshCount

  let knownFreshCount = solve2 ingredientDatabase
  putStrLn $ "Total known fresh ingredients: " <> show knownFreshCount