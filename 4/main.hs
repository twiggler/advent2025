import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Coord2 = (Int, Int)

type PaperRollCoords = HashSet Coord2

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

solve1 :: [[Cell]] -> Int
solve1 diagram =
  let paperRolls = mkPaperRolls diagram
      hasPaperRoll = (`HS.member` paperRolls)
      paperRollNeighbors =
        [ length $ filter hasPaperRoll neighborHood
        | coord <- HS.toList paperRolls,
          let neighborHood = neighbors coord
        ]
   in length $ filter (< 4) paperRollNeighbors

main :: IO ()
main = do
  (paperRollFile : _) <- getArgs
  paperRollDiagram <- parseFile readPaperRollDiagram paperRollFile
  let accessiblePaperRolls = solve1 paperRollDiagram
  putStrLn $ "Accessible paper rolls: " ++ show accessiblePaperRolls
