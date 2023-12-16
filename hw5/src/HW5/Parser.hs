module HW5.Parser
  ( parse
  ) where

import Control.Applicative (optional, many)
import Control.Applicative.Combinators (between, sepBy)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import HW5.Base (HiExpr (..), HiValue (..), HiFun(..), funName)
import Text.Megaparsec (MonadParsec (eof), Parsec, (<|>), runParser)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof expr) ""

expr :: Parser HiExpr
expr = makeExprParser exprTerm operatorTable

inBrackets :: Parser p -> Parser p
inBrackets inner = takeToken "(" *> inner <* takeToken ")"

exprTerm :: Parser HiExpr
exprTerm = do
  w <- optional $ inBrackets expr
  maybe exprTerm' return w

exprTerm' :: Parser HiExpr
exprTerm' = do
  object <- functionName <|> numeric
  args <- many functionArgs
  return $ foldl HiExprApply object args

lexeme :: Parser p -> Parser p
lexeme = L.lexeme space

takeToken :: String -> Parser String
takeToken = L.symbol space

numeric :: Parser HiExpr
numeric = lexeme $ HiExprValue . HiValueNumber . toRational <$> L.signed space L.scientific

functionName :: Parser HiExpr
functionName = support HiFunDiv
           <|> support HiFunMul
           <|> support HiFunAdd
           <|> support HiFunSub where
  support :: HiFun -> Parser HiExpr
  support f = lexeme $ HiExprValue . HiValueFunction . const f <$> takeToken (funName f)

functionArgs :: Parser [HiExpr]
functionArgs = inBrackets $ expr `sepBy` takeToken ","

-- TODO IN T3
operatorTable :: [[Operator Parser HiExpr]]
operatorTable = []
