module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , funName
  ) where

import qualified Data.Text as T

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
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  deriving (Show, Eq, Ord)

data HiValue =
    HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueString T.Text
  | HiValueFunction HiFun
  deriving (Show, Eq, Ord)

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

funName :: HiFun -> String
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
funName HiFunLength = "length"
funName HiFunToUpper = "to-upper"
funName HiFunToLower = "to-lower"
funName HiFunReverse = "reverse"
funName HiFunTrim = "trim"
