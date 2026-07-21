import Data.Bifunctor (first)
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as PQ
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Coord2 = (Int, Int)

type PaperRollCoords = HashSet Coord2

type PaperRollQueue = HashPSQ Coord2 Int [Coord2]

data Cell = PaperRoll | Empty deriving (Eq, Show)

readPaperRollDiagram :: Parser [[Cell]]
readPaperRollDiagram = do
  firstRow <- many cell <* eol
  let dim = length firstRow
  restRows <- count (dim - 1) (count dim cell <* eol) <* eof
  return (firstRow : restRows)
  where
    cell = (PaperRoll <$ char '@') <|> (Empty <$ char '.')

mkPaperRolls :: [[Cell]] -> PaperRollCoords
mkPaperRolls diagram =
  HS.fromList
    [ (x, y)
    | (y, row) <- zip [0 ..] diagram,
      (x, cell) <- zip [0 ..] row,
      cell == PaperRoll
    ]

neighbors :: Coord2 -> [Coord2]
neighbors = \(x, y) ->
  [(x + dx, y + dy) | (dx, dy) <- offsets]
  where
    offsets = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

adjacentPaperRolls :: PaperRollCoords -> Coord2 -> [Coord2]
adjacentPaperRolls paperRolls coord =
  filter (`HS.member` paperRolls) (neighbors coord)

adjustPaperRollQueue :: ((Int, [Coord2]) -> (Int, [Coord2])) -> Coord2 -> PaperRollQueue -> PaperRollQueue
adjustPaperRollQueue f k q = snd $ PQ.alter (((),) . fmap f) k q

solve1 :: [[Cell]] -> Int
solve1 diagram =
  let paperRolls = mkPaperRolls diagram
      paperRollNeighbors = [adjacentPaperRolls paperRolls coord | coord <- HS.toList paperRolls]
   in length $ filter (\adj -> length adj < 4) paperRollNeighbors

solve2 :: [[Cell]] -> Int
solve2 diagram =
  let paperRolls = mkPaperRolls diagram
      queue =
        PQ.fromList
          [ (coord, length adj, adj)
          | coord <- HS.toList paperRolls,
            let adj = adjacentPaperRolls paperRolls coord
          ]
   in length queue - length (go queue)
  where
    go queue =
      case PQ.atMostView 3 queue of
        ([], _) -> queue
        (removedRolls, queue') ->
          let lostNeighbor = [roll | (_, _, adj) <- removedRolls, roll <- adj]
              decreasePriority = flip $ adjustPaperRollQueue (first pred)
           in go (foldl' decreasePriority queue' lostNeighbor)

main :: IO ()
main = do
  (paperRollFile : _) <- getArgs
  paperRollDiagram <- parseFile readPaperRollDiagram paperRollFile
  let accessiblePaperRolls = solve1 paperRollDiagram
  putStrLn $ "Accessible paper rolls: " ++ show accessiblePaperRolls
  let removedPaperRolls = solve2 paperRollDiagram
  putStrLn $ "Removed paper rolls: " ++ show removedPaperRolls
