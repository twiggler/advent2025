{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except
import Data.Char (digitToInt, isDigit)
import Data.List (transpose, unsnoc)
import Data.List.Split qualified as S
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Number (assembleNumber)
import Parsing
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Exception (Exception)

data Operator = Add | Multiply deriving (Show)

data MathProblem = MathProblem
  { operands :: [Int],
    operator :: Operator
  }
  deriving (Show)

data MathParseError = NotEnoughLines | NotEnoughColumns | InvalidOperator

instance Show MathParseError where
  show NotEnoughLines = "Requires at least two lines of input: one for operands and one for operators."
  show NotEnoughColumns = "Requires at least one column of input."
  show InvalidOperator = "Encountered an unknown operator in the input."

instance Exception MathParseError

readMathHomework1 :: Parser [MathProblem]
readMathHomework1 =
  zipWith MathProblem
    <$> (transpose <$> endBy operandRow eol)
    <*> operatorRow
    <* eol
    <* eof
  where
    operandRow = hspace *> endBy L.decimal hspace
    operatorRow = hspace *> endBy (Add <$ char '+' <|> Multiply <$ char '*') hspace

readMathHomework2 :: (MonadError MathParseError m) => [Text] -> m [MathProblem]
readMathHomework2 content = do
  (operandLines, operatorLine) <- note NotEnoughLines (unsnoc $ T.unpack <$> content)
  let blocks =
        S.split
          (S.dropInitBlank . S.keepDelimsL $ S.mapSplitter fst $ S.oneOf "+*")
          (operatorLine `zip` transpose operandLines)
  traverse parseBlock blocks
  where
    parseBlock :: (MonadError MathParseError m) => [(Char, String)] -> m MathProblem
    parseBlock block = do
      operatorChar <- note NotEnoughColumns (fst <$> listToMaybe block)
      operator' <- note InvalidOperator (parseOperator operatorChar)
      let operands' = mapMaybe parseOperand (snd <$> block)
      return $ MathProblem operands' operator'
      where
        parseOperator ch = case ch of
          '+' -> Just Add
          '*' -> Just Multiply
          _ -> Nothing

        parseOperand column = case takeWhile isDigit . dropWhile (not . isDigit) $ column of
          [] -> Nothing
          xs -> Just $ assembleNumber $ digitToInt <$> xs

solveMathProblem :: MathProblem -> Int
solveMathProblem (MathProblem ops Add) = sum ops
solveMathProblem (MathProblem ops Multiply) = product ops

solve :: [MathProblem] -> Int
solve problems = sum $ solveMathProblem <$> problems

main :: IO ()
main = do
  (mathHomeworkFile : _) <- getArgs
  mathHomework <- parseFile readMathHomework1 mathHomeworkFile
  let grandTotal = solve mathHomework
  putStrLn $ "The grand total of all problems is: " <> show grandTotal

  mathHomework2 <- parseFileLines readMathHomework2 mathHomeworkFile
  let grandTotal2 = solve mathHomework2
  putStrLn $ "The grand total of all problems Celaphod style is: " <> show grandTotal2
