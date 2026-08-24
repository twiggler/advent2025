{-# LANGUAGE RankNTypes #-}

module Parsing
  ( Parser,
    parseFile,
    parseFileLines,
    note,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.Except (MonadError, throwError)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec

type Parser a = Parsec Void Text a

newtype FatalParseError s e = FatalParseError (ParseErrorBundle s e)

instance (VisualStream s, TraversableStream s, ShowErrorComponent e) => Show (FatalParseError s e) where
  show (FatalParseError err) = errorBundlePretty err

type LinesParser e a = forall m. (MonadError e m) => [Text] -> m a

parseFile :: Parser a -> FilePath -> IO a
parseFile parser filename = do
  contents <- TIO.readFile filename
  case runParser parser filename contents of
    Left err -> throwIO err
    Right result -> return result

parseFileLines :: (Exception e) => LinesParser e a -> FilePath -> IO a
parseFileLines parser filename = do
  contents <- T.lines <$> TIO.readFile filename
  case parser contents of
    Left err -> throwIO err
    Right result -> return result

note :: (MonadError e m) => e -> Maybe a -> m a
note err = maybe (throwError err) pure
