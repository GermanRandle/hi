module HW5.Evaluator
  ( eval
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Except (runExceptT)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Text as T
import HW5.Base (HiError (..), HiExpr (..), HiFun(..), HiValue (..))

type ArgTaker m a = HiValue -> Evaluator m a
type BinaryFunction a b c = a -> b -> c
type Evaluator = ExceptT HiError
type UnaryFunction a b = a -> b

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ eval' expr

eval' :: Monad m => HiExpr -> Evaluator m HiValue
eval' (HiExprValue x) = return x
eval' (HiExprApply f args) = evalApply f args

evalApply :: Monad m => HiExpr -> [HiExpr] -> Evaluator m HiValue
evalApply object args = do
  eObject <- eval' object
  eArgs <- mapM eval' args
  case eObject of
    (HiValueFunction f) -> evalFunc f eArgs
    (HiValueString s) -> stringIndex s eArgs
    _ -> throwError HiErrorInvalidFunction

evalFunc :: Monad m => HiFun -> [HiValue] -> Evaluator m HiValue
evalFunc f [a] = evalFuncUnary f a
evalFunc f [a, b] = evalFuncBinary f a b
evalFunc f [a, b, c] = evalFuncTernary f a b c
evalFunc _ _ = throwError HiErrorArityMismatch

evalFuncUnary :: Monad m => HiFun -> HiValue -> Evaluator m HiValue
evalFuncUnary HiFunNot = evalFuncUnary' takeBool not HiValueBool
evalFuncUnary HiFunLength = evalFuncUnary' takeText (toRational . T.length) HiValueNumber
evalFuncUnary HiFunToUpper = evalFuncUnary' takeText T.toUpper HiValueString
evalFuncUnary HiFunToLower = evalFuncUnary' takeText T.toLower HiValueString
evalFuncUnary HiFunReverse = evalFuncUnary' takeText T.reverse HiValueString
evalFuncUnary HiFunTrim = evalFuncUnary' takeText T.strip HiValueString
evalFuncUnary _ = do const (throwError HiErrorArityMismatch)

evalFuncUnary' :: Monad m => ArgTaker m a -> UnaryFunction a b -> (b -> HiValue) -> HiValue -> Evaluator m HiValue
evalFuncUnary' argTaker f resWrapper a = do
  ea <- argTaker a
  return $ resWrapper $ f ea

evalFuncBinary, evalFuncBinaryPolymorphic :: Monad m => HiFun -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary HiFunSub = evalFuncBinary' takeNum takeNum (-) HiValueNumber
evalFuncBinary HiFunAnd = evalFuncBinary' takeBool takeBool (&&) HiValueBool
evalFuncBinary HiFunOr = evalFuncBinary' takeBool takeBool (||) HiValueBool
evalFuncBinary HiFunLessThan = evalFuncBinary' return return  (<) HiValueBool
evalFuncBinary HiFunGreaterThan = evalFuncBinary' return return (>) HiValueBool
evalFuncBinary HiFunEquals = evalFuncBinary' return return (==) HiValueBool
evalFuncBinary HiFunNotLessThan = evalFuncBinary' return return (>=) HiValueBool
evalFuncBinary HiFunNotGreaterThan = evalFuncBinary' return return (<=) HiValueBool
evalFuncBinary HiFunNotEquals = evalFuncBinary' return return (/=) HiValueBool
evalFuncBinary f = evalFuncBinaryPolymorphic f

evalFuncBinaryPolymorphic HiFunAdd a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (+) HiValueNumber a
evalFuncBinaryPolymorphic HiFunAdd a@(HiValueString _) = evalFuncBinary' takeText takeText (<>) HiValueString a
evalFuncBinaryPolymorphic HiFunDiv a@(HiValueNumber _) = evalFuncBinary' takeNum takeDivisor (/) HiValueNumber a
evalFuncBinaryPolymorphic HiFunDiv a@(HiValueString _) = evalFuncBinary' takeText takeText (\ x y -> T.snoc x '/' <> y) HiValueString a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (*) HiValueNumber a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueString _) = evalFuncBinary' takeText takeNatural (flip stimes) HiValueString a
evalFuncBinaryPolymorphic _ _ = do const (throwError HiErrorArityMismatch)

evalFuncBinary' :: Monad m => ArgTaker m a -> ArgTaker m b -> BinaryFunction a b c -> (c -> HiValue) -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary' argTaker1 argTaker2 f resWrapper a b = do
  ea <- argTaker1 a
  eb <- argTaker2 b
  return $ resWrapper $ f ea eb

evalFuncTernary :: Monad m => HiFun -> HiValue -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncTernary HiFunIf cond thenBranch elseBranch = do
  eCond <- takeBool cond
  return $ if eCond then thenBranch else elseBranch
evalFuncTernary _ _ _ _ = throwError HiErrorArityMismatch

stringIndex :: Monad m => T.Text -> [HiValue] -> Evaluator m HiValue
stringIndex s [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ if idx >= 0 && T.compareLength s idx == LT
           then HiValueString $ T.singleton $ s `T.index` idx
           else HiValueNull
stringIndex s [el1, el2] = do
  res <- slice (T.unpack s) el1 el2
  return $ HiValueString $ T.pack res
stringIndex _ _ = throwError HiErrorArityMismatch

slice :: Monad m => [a] -> HiValue -> HiValue -> Evaluator m [a]
slice lst HiValueNull r@(HiValueNumber _) = slice lst (HiValueNumber 0) r
slice lst l@(HiValueNumber _) HiValueNull = slice lst l (HiValueNumber (toRational $ length lst))
slice lst l@(HiValueNumber _) r@(HiValueNumber _) = do
  l' <- normalize . fromIntegral <$> takeInteger l
  r' <- normalize . fromIntegral <$> takeInteger r
  return $ take (r' - l') (drop l' lst)
  where
    normalize :: Int -> Int
    normalize idx = if idx < 0 then idx + length lst else idx
slice _ _ _ = throwError HiErrorInvalidArgument

-- ArgTakers

takeNum, takeDivisor :: Monad m => ArgTaker m Rational
takeNum (HiValueNumber x) = return x
takeNum _ = throwError HiErrorInvalidArgument

takeDivisor val = do
  d <- takeNum val
  if d == 0 then throwError HiErrorDivideByZero else return d

takeBool :: Monad m => ArgTaker m Bool
takeBool (HiValueBool b) = return b
takeBool _ = throwError HiErrorInvalidArgument

takeText :: Monad m => ArgTaker m T.Text
takeText (HiValueString t) = return t
takeText _ = throwError HiErrorInvalidArgument

takeInteger, takeNatural :: Monad m => ArgTaker m Integer
takeInteger val = do
  x <- takeNum val
  if denominator x == 1
    then return $ numerator x
    else throwError HiErrorInvalidArgument

takeNatural val = do
  x <- takeInteger val
  if x >= 0 
    then return x
    else throwError HiErrorInvalidArgument
