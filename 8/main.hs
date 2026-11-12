import Data.List
import Data.Ord (Down (Down))
import Data.DisjointSet qualified as DS
import Linear (V3 (..), qd)
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

circuitsToForm :: Int
circuitsToForm = 3

readJunctionPositions :: Parser [V3 Int]
readJunctionPositions = endBy position eol <* eof
  where
    position = V3 <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

solve1 :: [V3 Int] -> Int -> Int
solve1 junctions junctionsToConnect =
  let pairs = [(j1, j2) | j1 : js <- tails junctions, j2 <- js]
      candidatePairs = take junctionsToConnect $ sortOn (uncurry qd) pairs
      circuits = foldl' (\ds (j1, j2) -> DS.union j1 j2 ds) DS.empty candidatePairs
      circuitSizes = length <$> DS.toLists circuits
   in product $ take circuitsToForm $ sortOn Down circuitSizes

main :: IO ()
main = do
  (junctionFile : closestJunctionPairsStr : _) <- getArgs
  junctionPositions <- parseFile readJunctionPositions junctionFile
  let productCircuitSizes = solve1 junctionPositions (read closestJunctionPairsStr)
  putStrLn $ "Product of three largest circuits is: " <> show productCircuitSizes
