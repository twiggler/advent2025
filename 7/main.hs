import Control.Monad (mfilter)
import Data.Functor ((<&>))
import Data.HashSet as HS
import Data.IntMap.Strict as IM
import Data.IntSet as IS
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)

data Cell = Empty | Splitter | Start deriving (Eq)

readTachyonManifold :: Parser [[Cell]]
readTachyonManifold = endBy (many cell) eol <* eof
  where
    cell = (Empty <$ char '.') <|> (Splitter <$ char '^') <|> (Start <$ char 'S')

solve1 :: [[Cell]] -> Int
solve1 grid = HS.size $ go entrances HS.empty
  where
    cellWithCoords = [(x, y, cell) | (y, row) <- zip [0 ..] grid, (x, cell) <- zip [0 ..] row]

    entrances = [(x, y) | (x, y, cell) <- cellWithCoords, cell == Start]

    splitterYByX =
      IM.fromListWith IS.union [(x, IS.singleton y) | (x, y, cell) <- cellWithCoords, cell == Splitter]

    go (pos : rest) visited
      | pos `HS.member` visited = go rest visited
      | otherwise = case mfilter unexplored (castBeam pos) of
          Just (splitterX, splitterY) ->
            let children = [(splitterX - 1, splitterY), (splitterX + 1, splitterY)]
             in go (children ++ rest) (HS.insert (splitterX, splitterY) visited)
          Nothing -> go rest visited
      where
        unexplored (x, y) = not $ (x, y) `HS.member` visited
        castBeam (x, y) = IM.lookup x splitterYByX >>= IS.lookupGT y <&> (x,)
    go [] visited = visited

main :: IO ()
main = do
  (tachyonManifoldFile : _) <- getArgs
  tachyonManifold <- parseFile readTachyonManifold tachyonManifoldFile
  let numberOfSplits = solve1 tachyonManifold
  putStrLn $ "Number of splits: " ++ show numberOfSplits
