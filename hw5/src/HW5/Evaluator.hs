module HW5.Evaluator
  ( eval
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Except (runExceptT)
import HW5.Base (HiError (..), HiExpr (..), HiFun(..), HiValue (..))

type ArgTaker m a = HiValue -> Evaluator m a
type BinaryFunction a b c = a -> b -> c
type Evaluator = ExceptT HiError

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
    _ -> throwError HiErrorInvalidFunction

evalFunc :: Monad m => HiFun -> [HiValue] -> Evaluator m HiValue
evalFunc f [a] = evalFuncUnary f a
evalFunc f [a, b] = evalFuncBinary f a b
evalFunc f [a, b, c] = evalFuncTernary f a b c
evalFunc _ _ = throwError HiErrorArityMismatch

evalFuncUnary :: Monad m => HiFun -> HiValue -> Evaluator m HiValue
evalFuncUnary HiFunNot a = do
  ea <- takeBool a
  return $ HiValueBool $ not ea
evalFuncUnary _ _ = throwError HiErrorArityMismatch

evalFuncBinary :: Monad m => HiFun -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary HiFunDiv = evalFuncBinary' takeNum takeDivisor (/) HiValueNumber
evalFuncBinary HiFunMul = evalFuncBinary' takeNum takeNum (*) HiValueNumber
evalFuncBinary HiFunAdd = evalFuncBinary' takeNum takeNum (+) HiValueNumber
evalFuncBinary HiFunSub = evalFuncBinary' takeNum takeNum (-) HiValueNumber
evalFuncBinary HiFunAnd = evalFuncBinary' takeBool takeBool (&&) HiValueBool
evalFuncBinary HiFunOr = evalFuncBinary' takeBool takeBool (||) HiValueBool
evalFuncBinary HiFunLessThan = evalFuncBinary' return return  (<) HiValueBool
evalFuncBinary HiFunGreaterThan = evalFuncBinary' return return (>) HiValueBool
evalFuncBinary HiFunEquals = evalFuncBinary' return return (==) HiValueBool
evalFuncBinary HiFunNotLessThan = evalFuncBinary' return return (>=) HiValueBool
evalFuncBinary HiFunNotGreaterThan = evalFuncBinary' return return (<=) HiValueBool
evalFuncBinary HiFunNotEquals = evalFuncBinary' return return (/=) HiValueBool
evalFuncBinary _ = do const (do const (throwError HiErrorArityMismatch))

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

-- ArgTakers

takeNum :: Monad m => HiValue -> Evaluator m Rational
takeNum (HiValueNumber x) = return x
takeNum _ = throwError HiErrorInvalidArgument

takeBool :: Monad m => HiValue -> Evaluator m Bool
takeBool (HiValueBool b) = return b
takeBool _ = throwError HiErrorInvalidArgument

takeDivisor :: Monad m => HiValue -> Evaluator m Rational
takeDivisor val = do
  d <- takeNum val
  if d == 0 then throwError HiErrorDivideByZero else return d
