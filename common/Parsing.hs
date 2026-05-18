module Parsing
  ( Parser,
    parseFile,
  )
where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import System.Exit (die)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

type Parser a = Parsec Void Text a

parseFile :: Parser a -> FilePath -> IO a
parseFile parser filename = do
  contents <- TIO.readFile filename
  case runParser parser filename contents of
    Left err -> die $ errorBundlePretty err
    Right result -> return result
