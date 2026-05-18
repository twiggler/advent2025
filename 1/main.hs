import Data.List (mapAccumL, scanl')
import Parsing (Parser, parseFile)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

maxDial, startDial :: Int
maxDial = 100
startDial = 50

readRotations :: Parser [Int]
readRotations = rotation `endBy` eol <* eof
  where
    rotation = left <|> right
    left = char 'L' *> (negate <$> L.decimal)
    right = char 'R' *> L.decimal

solve1 :: [Int] -> Int
solve1 =
  length . filter (== 0) . scanl' turn startDial
  where
    turn pos rot = (pos + rot) `mod` maxDial

solve2 :: [Int] -> Int
solve2 =
  sum . snd . mapAccumL turn startDial
  where
    turn pos rot = (pos', crossings)
      where
        pos' = (pos + rot) `mod` maxDial
        crossings
          | rot >= 0 = (pos + rot) `div` maxDial
          | otherwise =
              let cross = pos > 0 && abs rot `rem` maxDial >= pos
               in abs rot `div` maxDial + if cross then 1 else 0

main :: IO ()
main = do
  (rotationsFile : _) <- getArgs
  rotations <- parseFile readRotations rotationsFile
  let timesDialAtZero = solve1 rotations
  putStrLn $ "The dial is at zero " ++ show timesDialAtZero ++ " times. (method 1)"
  let timesDialAtZero' = solve2 rotations
  putStrLn $ "The dial is at zero " ++ show timesDialAtZero' ++ " times. (method 2)"
