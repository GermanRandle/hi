module HW5.Parser
  ( parse
  ) where

import Control.Applicative (optional, many)
import Control.Applicative.Combinators (between, sepBy)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (toLower)
import Data.Void (Void)
import HW5.Base (HiExpr (..), HiValue (..), HiFun(..), funName)
import Text.Megaparsec (MonadParsec (eof), Parsec, (<|>), notFollowedBy, runParser, try)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof expr) ""

expr :: Parser HiExpr
expr = makeExprParser exprTerm operatorTable

inParentheses :: Parser p -> Parser p
inParentheses inner = takeToken "(" *> inner <* takeToken ")"

exprTerm :: Parser HiExpr
exprTerm = do
  w <- optional $ inParentheses expr
  maybe exprTerm' return w

exprTerm' :: Parser HiExpr
exprTerm' = do
  object <- functionName <|> numeric <|> boolean
  args <- many functionArgs
  return $ foldl HiExprApply object args

lexeme :: Parser p -> Parser p
lexeme = L.lexeme space

takeToken :: String -> Parser String
takeToken = L.symbol space

takeTokenNotFollowedBy :: String -> String -> Parser String
takeTokenNotFollowedBy token follow = try $ takeToken token <* (notFollowedBy . takeToken) follow

numeric :: Parser HiExpr
numeric = lexeme $ HiExprValue . HiValueNumber . toRational <$> L.signed space L.scientific

boolean :: Parser HiExpr
boolean = lexeme $ support False <|> support True where
  support :: Bool -> Parser HiExpr
  support b = HiExprValue . HiValueBool . const b <$> takeToken (map toLower (show b))

functionName :: Parser HiExpr
functionName = lexeme $ support HiFunDiv
                    <|> support HiFunMul
                    <|> support HiFunAdd
                    <|> support HiFunSub
                    <|> support HiFunNotLessThan
                    <|> support HiFunNotGreaterThan
                    <|> support HiFunNotEquals
                    <|> support HiFunNot
                    <|> support HiFunAnd
                    <|> support HiFunOr
                    <|> support HiFunLessThan
                    <|> support HiFunGreaterThan
                    <|> support HiFunEquals
                    <|> support HiFunIf where
  support :: HiFun -> Parser HiExpr
  support f = HiExprValue . HiValueFunction . const f <$> takeToken (funName f)

functionArgs :: Parser [HiExpr]
functionArgs = inParentheses $ expr `sepBy` takeToken ","

-- TODO IN T3
operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [ [  InfixL $ supportExpr HiFunDiv <$ takeTokenNotFollowedBy "/" "="
                  , supportBinaryInfixL "*" HiFunMul ]
                , [ supportBinaryInfixL "+" HiFunAdd
                  , supportBinaryInfixL "-" HiFunSub ]
                , [ supportBinaryInfixN "<=" HiFunNotGreaterThan
                  , supportBinaryInfixN ">=" HiFunNotLessThan
                  , supportBinaryInfixN "<" HiFunLessThan
                  , supportBinaryInfixN ">" HiFunGreaterThan
                  , supportBinaryInfixN "==" HiFunEquals
                  , supportBinaryInfixN "/=" HiFunNotEquals ]
                , [ supportBinaryInfixR "&&" HiFunAnd ]
                , [ supportBinaryInfixR "||" HiFunOr ] ] where
  supportBinaryInfixL :: String -> HiFun -> Operator Parser HiExpr
  supportBinaryInfixL token constr = InfixL $ supportExpr constr <$ takeToken token

  supportBinaryInfixR :: String -> HiFun -> Operator Parser HiExpr
  supportBinaryInfixR token constr = InfixR $ supportExpr constr <$ takeToken token

  supportBinaryInfixN :: String -> HiFun -> Operator Parser HiExpr
  supportBinaryInfixN token constr = InfixN $ supportExpr constr <$ takeToken token

  supportExpr :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
  supportExpr f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]
