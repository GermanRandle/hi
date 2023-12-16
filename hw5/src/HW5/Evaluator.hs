module HW5.Evaluator
  ( eval
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Except (runExceptT)
import HW5.Base (HiError (..), HiExpr (..), HiFun(..), HiValue (..))

-- TODO: type for calculation result
eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ eval' expr

eval' :: Monad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue x) = return x
eval' (HiExprApply f args) = evalApply f args

evalApply :: Monad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
evalApply f args = do
  eObject <- eval' f
  eArgs <- evalArgs args
  evalApply' eObject eArgs

evalArgs :: Monad m => [HiExpr] -> ExceptT HiError m [HiValue]
evalArgs args = do mapM eval' args

evalApply' :: Monad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
evalApply' f args =
  case f of
    (HiValueFunction func) -> do
      if length args /= 2
      then throwError HiErrorArityMismatch
      else do
        numberArgs <- mapM evalNumericArg args
        evalApplyBinary func (head numberArgs) (numberArgs !! 1)
    _ -> throwError HiErrorInvalidFunction

evalNumericArg :: Monad m => HiValue -> ExceptT HiError m Rational
evalNumericArg (HiValueNumber x) = return x
evalNumericArg _ = throwError HiErrorInvalidArgument

evalApplyBinary :: Monad m => HiFun -> Rational -> Rational -> ExceptT HiError m HiValue
evalApplyBinary HiFunDiv a b = if b == 0 then throwError HiErrorDivideByZero else return $ HiValueNumber $ a / b
evalApplyBinary HiFunMul a b = return $ HiValueNumber $ a * b
evalApplyBinary HiFunAdd a b = return $ HiValueNumber $ a + b
evalApplyBinary HiFunSub a b = return $ HiValueNumber $ a - b
