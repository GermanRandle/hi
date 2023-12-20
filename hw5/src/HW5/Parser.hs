module HW5.Parser
  ( parse
  ) where

import Control.Applicative (optional, many)
import Control.Applicative.Combinators (between, sepBy, sepBy1)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum, toLower)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiExpr (..), HiValue (..), HiFun(..), funName)
import Text.Megaparsec (MonadParsec (eof), Parsec, (<|>), manyTill, notFollowedBy, runParser, satisfy, sepEndBy, try)
import Text.Megaparsec.Char (char, hexDigitChar, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between space eof expr) ""

expr :: Parser HiExpr
expr = makeExprParser exprTerm operatorTable

exprTerm :: Parser HiExpr
exprTerm = do
  obj <- exprObject
  postActions <- many objPostAction
  return $ foldl (\ee f -> f ee) obj postActions

objPostAction :: Parser (HiExpr -> HiExpr)
objPostAction = (HiExprRun <$ takeToken "!")
         <|> (flip HiExprApply . (: []) <$> dotAccess)
         <|> (flip HiExprApply <$> functionArgs)

exprObject :: Parser HiExpr
exprObject = do
  w <- optional $ inParentheses expr
  maybe exprObject' return w

exprObject' :: Parser HiExpr
exprObject' = (HiExprValue <$> value) <|> listLiteral <|> dictLiteral

numeric :: Parser HiValue
numeric = lexeme $ HiValueNumber . toRational <$> L.signed space L.scientific

boolean :: Parser HiValue
boolean = support False <|> support True 
  where
    support :: Bool -> Parser HiValue
    support b = HiValueBool . const b <$> takeToken (map toLower (show b))

nullKeyword :: Parser HiValue
nullKeyword = HiValueNull <$ takeToken "null"

cwdKeyword :: Parser HiValue
cwdKeyword = HiValueAction HiActionCwd <$ takeToken "cwd"

nowKeyword :: Parser HiValue
nowKeyword = HiValueAction HiActionNow <$ takeToken "now"

stringLiteral :: Parser HiValue
stringLiteral = lexeme $ HiValueString . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

byteArrayLiteral :: Parser HiValue
byteArrayLiteral = HiValueBytes . B.pack <$> (takeToken "[#" *> (hex2 `sepEndBy` space) <* takeToken "#]") 
  where
    hex2 :: Parser Word8
    hex2 = do
      first <- hexDigitChar
      second <- hexDigitChar
      return $ fromInteger $ read $ "0x" ++ [first, second]

value :: Parser HiValue
value = functionName 
    <|> numeric
    <|> boolean
    <|> nullKeyword
    <|> stringLiteral
    <|> byteArrayLiteral
    <|> cwdKeyword
    <|> nowKeyword

listLiteral :: Parser HiExpr
listLiteral = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> (takeToken "[" *> (expr `sepBy` takeToken ",") <* takeToken "]")

dictLiteral :: Parser HiExpr
dictLiteral = HiExprDict <$> (takeToken "{" *> (entry `sepBy` takeToken ",") <* takeToken "}") 
  where
    entry :: Parser (HiExpr, HiExpr)
    entry = do
      key <- expr <* takeToken ":"
      val <- expr
      return (key, val)

functionArgs :: Parser [HiExpr]
functionArgs = inParentheses $ expr `sepBy` takeToken ","

dotAccess :: Parser HiExpr
dotAccess = lexeme $ HiExprValue . HiValueString . T.pack <$> (char '.' *> keyIdentifier)
  where
    keyIdentifier :: Parser String
    keyIdentifier = concat <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')

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
  <|> support HiFunFold 
  <|> support HiFunPackBytes
  <|> support HiFunUnpackBytes
  <|> support HiFunEncodeUtf8
  <|> support HiFunDecodeUtf8
  <|> support HiFunZip
  <|> support HiFunUnzip
  <|> support HiFunSerialise
  <|> support HiFunDeserialise 
  <|> support HiFunRead
  <|> support HiFunWrite
  <|> support HiFunMkDir
  <|> support HiFunChDir
  <|> support HiFunParseTime
  <|> support HiFunRand
  <|> support HiFunEcho
  <|> support HiFunCount
  <|> support HiFunValues
  <|> support HiFunKeys
  <|> support HiFunInvert 
    where
      support :: HiFun -> Parser HiValue
      support f = HiValueFunction . const f <$> takeToken (funName f)

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
  , [ supportSign "||" HiFunOr InfixR ] ]
    where
      supportSign :: String -> HiFun -> (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> Operator Parser HiExpr
      supportSign token fConstr assoc = assoc $ supportFun fConstr <$ takeToken token

      supportFun :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
      supportFun f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

-- Auxiliary

lexeme, inParentheses :: Parser p -> Parser p
lexeme              = L.lexeme space
inParentheses inner = takeToken "(" *> inner <* takeToken ")"

takeToken :: String -> Parser String
takeToken = L.symbol space

takeTokenNotFollowedBy :: String -> String -> Parser String
takeTokenNotFollowedBy token follow = try $ takeToken token <* (notFollowedBy . takeToken) follow
