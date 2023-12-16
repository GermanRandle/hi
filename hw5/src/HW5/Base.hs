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
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  deriving Show

data HiValue =
    HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
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
funName HiFunNot = "not"
funName HiFunAnd = "and"
funName HiFunOr = "or"
funName HiFunLessThan = "less-than"
funName HiFunGreaterThan = "greater-than"
funName HiFunEquals = "equals"
funName HiFunNotLessThan = "not-less-than"
funName HiFunNotGreaterThan = "not-greater-than"
funName HiFunNotEquals = "not-equals"
funName HiFunIf = "if"
