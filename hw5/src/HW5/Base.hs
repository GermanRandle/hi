module HW5.Base
  ( HiError
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  ) where

data HiFun =
    HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  deriving Show

data HiValue =
    HiValueNumber Rational
  | HiValueFunction HiFun
  deriving Show

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving Show

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving Show
