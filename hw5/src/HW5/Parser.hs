module HW5.Parser
  ( parse
  ) where

import Control.Applicative (optional, many)
import Control.Applicative.Combinators (between, sepBy)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (toLower)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Void (Void)
import HW5.Base (HiExpr (..), HiValue (..), HiFun(..), funName)
import Text.Megaparsec (MonadParsec (eof), Parsec, (<|>), manyTill, notFollowedBy, runParser, try)
import Text.Megaparsec.Char (char, space)
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
  object <- HiExprValue <$> value
  args <- many functionArgs
  return $ foldl HiExprApply object args

lexeme :: Parser p -> Parser p
lexeme = L.lexeme space

takeToken :: String -> Parser String
takeToken = L.symbol space

takeTokenNotFollowedBy :: String -> String -> Parser String
takeTokenNotFollowedBy token follow = try $ takeToken token <* (notFollowedBy . takeToken) follow

numeric :: Parser HiValue
numeric = lexeme $ HiValueNumber . toRational <$> L.signed space L.scientific

boolean :: Parser HiValue
boolean = lexeme $ support False <|> support True where
  support :: Bool -> Parser HiValue
  support b = HiValueBool . const b <$> takeToken (map toLower (show b))

nullKeyword :: Parser HiValue
nullKeyword = lexeme $ HiValueNull <$ takeToken "null" -- excessive lexemes?

stringLiteral :: Parser HiValue
stringLiteral = lexeme $ HiValueString . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

listLiterals :: Parser HiValue
listLiterals = HiValueList . S.fromList <$> (takeToken "[" *> (value `sepBy` takeToken ",") <* takeToken "]")

value :: Parser HiValue
value = functionName <|> numeric <|> boolean <|> nullKeyword <|> stringLiteral <|> listLiterals

functionName :: Parser HiValue
functionName = lexeme $ 
      support HiFunDiv
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
  <|> support HiFunIf
  <|> support HiFunLength
  <|> support HiFunToUpper
  <|> support HiFunToLower
  <|> support HiFunReverse
  <|> support HiFunTrim
  <|> support HiFunList
  <|> support HiFunRange
  <|> support HiFunFold where
    support :: HiFun -> Parser HiValue
    support f = HiValueFunction . const f <$> takeToken (funName f)

functionArgs :: Parser [HiExpr]
functionArgs = inParentheses $ expr `sepBy` takeToken ","

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = 
  [ [  InfixL $ supportFun HiFunDiv <$ takeTokenNotFollowedBy "/" "="
    , supportSign "*" HiFunMul InfixL ]
  , [ supportSign "+" HiFunAdd InfixL
    , supportSign "-" HiFunSub InfixL ]
  , [ supportSign "<=" HiFunNotGreaterThan InfixN
    , supportSign ">=" HiFunNotLessThan InfixN
    , supportSign "<" HiFunLessThan InfixN
    , supportSign ">" HiFunGreaterThan InfixN
    , supportSign "==" HiFunEquals InfixN
    , supportSign "/=" HiFunNotEquals InfixN ]
  , [ supportSign "&&" HiFunAnd InfixR ]
  , [ supportSign "||" HiFunOr InfixR ] ] where
  supportSign :: String -> HiFun -> (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> Operator Parser HiExpr
  supportSign token fConstr assoc = assoc $ supportFun fConstr <$ takeToken token

  supportFun :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
  supportFun f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]
