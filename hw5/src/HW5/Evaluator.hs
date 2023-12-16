module HW5.Evaluator
  ( eval
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Except (runExceptT)
import HW5.Base (HiError (..), HiExpr (..), HiFun(..), HiValue (..))

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
    (HiValueFunction f) -> do
      if length eArgs /= 2
      then throwError HiErrorArityMismatch
      else do
        numberArgs <- mapM evalNumericArg eArgs
        evalApplyBinary f (head numberArgs) (numberArgs !! 1)
    _ -> throwError HiErrorInvalidFunction

evalNumericArg :: Monad m => HiValue -> Evaluator m Rational
evalNumericArg (HiValueNumber x) = return x
evalNumericArg _ = throwError HiErrorInvalidArgument

evalApplyBinary :: Monad m => HiFun -> Rational -> Rational -> Evaluator m HiValue
evalApplyBinary HiFunDiv a b = if b == 0 then throwError HiErrorDivideByZero else return $ HiValueNumber $ a / b
evalApplyBinary HiFunMul a b = return $ HiValueNumber $ a * b
evalApplyBinary HiFunAdd a b = return $ HiValueNumber $ a + b
evalApplyBinary HiFunSub a b = return $ HiValueNumber $ a - b
