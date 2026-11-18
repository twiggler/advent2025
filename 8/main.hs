import Data.DisjointSet (DisjointSet)
import Data.DisjointSet qualified as DS
import Data.List
import Data.Ord (Down (Down))
import Linear (V3 (..), qd)
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

circuitsToForm :: Int
circuitsToForm = 3

data Step = Step
  { junctionPair :: (V3 Int, V3 Int),
    circuits :: DisjointSet (V3 Int)
  }

readJunctionPositions :: Parser [V3 Int]
readJunctionPositions = endBy position eol <* eof
  where
    position = V3 <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

connectJunctions :: [V3 Int] -> [Step]
connectJunctions junctions =
  let pairs = sortOn (uncurry qd) [(j1, j2) | j1 : js <- tails junctions, j2 <- js]
      initial = foldMap DS.singleton junctions
      steps = scanl' (\ds (j1, j2) -> DS.union j1 j2 ds) initial pairs
   in zipWith Step pairs (drop 1 steps)

solve1 :: Int -> [Step] -> Maybe Int
solve1 nPairs steps = productOfLengths . circuits <$> steps !? (nPairs - 1)
  where
    productOfLengths = product . take circuitsToForm . sortOn Down . fmap length . DS.toLists

solve2 :: [Step] -> Maybe Int
solve2 = fmap (xProduct . junctionPair) . find (allConnected . circuits)
  where
    allConnected circuit = DS.sets circuit == 1
    xProduct (V3 x1 _ _, V3 x2 _ _) = x1 * x2

main :: IO ()
main = do
  (junctionFile : numberOfPairsStr : _) <- getArgs
  junctionPositions <- parseFile readJunctionPositions junctionFile

  let partialCircuits = connectJunctions junctionPositions

  case solve1 (read numberOfPairsStr) partialCircuits of
    Just sizeProduct -> putStrLn $ "Product of three largest circuits is: " <> show sizeProduct
    Nothing -> putStrLn "Not enough junctions to connect the required number of pairs"

  case solve2 partialCircuits of
    Just xProduct -> putStrLn $ "X product of last connected pair equals: " <> show xProduct
    Nothing -> putStrLn "No singleton circuit found"
