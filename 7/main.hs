import Control.Monad (mfilter)
import Data.Functor ((<&>))
import Data.HashMap.Lazy qualified as HM
import Data.HashSet qualified as HS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

data Cell = Empty | Splitter | Start

type Coord = (Int, Int)

data TachyonManifold = TachyonManifold
  { splitterYByX :: IntMap IntSet,
    entrances :: [Coord]
  }

readTachyonGrid :: Parser [[Cell]]
readTachyonGrid = endBy (many cell) eol <* eof
  where
    cell = (Empty <$ char '.') <|> (Splitter <$ char '^') <|> (Start <$ char 'S')

mkTachyonManifold :: [[Cell]] -> TachyonManifold
mkTachyonManifold grid =
  TachyonManifold
    { splitterYByX =
        IM.fromListWith
          IS.union
          [ (x, IS.singleton y) | (x, y, Splitter) <- cellWithCoords
          ],
      entrances = [(x, y) | (x, y, Start) <- cellWithCoords]
    }
  where
    cellWithCoords = [(x, y, cell) | (y, row) <- zip [0 ..] grid, (x, cell) <- zip [0 ..] row]

castBeam :: IntMap IntSet -> Coord -> Maybe Coord
castBeam splitterYByX' (x, y) = IM.lookup x splitterYByX' >>= IS.lookupGT y <&> (x,)

splitParticle :: Coord -> [Coord]
splitParticle (x, y) = [(x - 1, y), (x + 1, y)]

solve1 :: TachyonManifold -> Int
solve1 (TachyonManifold splitterYByX' entrances') = HS.size $ go entrances' HS.empty
  where
    go (pos : rest) visited =
      case mfilter unexplored (castBeam splitterYByX' pos) of
        Just splitter ->
          let children = splitParticle splitter
           in go (children ++ rest) (HS.insert splitter visited)
        Nothing -> go rest visited
      where
        unexplored (x, y) = not $ (x, y) `HS.member` visited
    go [] visited = visited

solve2 :: TachyonManifold -> Int
solve2 (TachyonManifold splitterYByX' entrances') = sum $ countTimelines <$> entrances'
  where
    splitters = [(x, y) | (x, ys) <- IM.toList splitterYByX', y <- IS.toList ys]

    timelinesBySplitter = HM.fromList [(s, sum (countTimelines <$> splitParticle s)) | s <- splitters]
    countTimelines coord = maybe 1 (timelinesBySplitter HM.!) (castBeam splitterYByX' coord)

main :: IO ()
main = do
  (tachyonManifoldFile : _) <- getArgs
  tachyonGrid <- parseFile readTachyonGrid tachyonManifoldFile
  let tachyonManifold = mkTachyonManifold tachyonGrid
  let numberOfSplits = solve1 tachyonManifold
  putStrLn $ "Number of splits: " <> show numberOfSplits
  let numberOfTimelines = solve2 tachyonManifold
  putStrLn $ "Number of timelines: " <> show numberOfTimelines
