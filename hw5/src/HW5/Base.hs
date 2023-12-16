module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , funName
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

funName :: HiFun -> String -- TODO: make a model with all function attributes?
funName HiFunDiv = "div"
funName HiFunMul = "mul"
funName HiFunAdd = "add"
funName HiFunSub = "sub"
