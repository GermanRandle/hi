{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , funName
  ) where

import Codec.Serialise (Serialise)
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import qualified Data.Text as T
import GHC.Generics (Generic)

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
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiValue =
    HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueString T.Text
  | HiValueFunction HiFun
  | HiValueList (S.Seq HiValue)
  | HiValueBytes B.ByteString
  deriving (Show, Eq, Ord, Generic, Serialise)

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
funName HiFunList = "list"
funName HiFunRange = "range"
funName HiFunFold = "fold"
funName HiFunPackBytes = "pack-bytes"
funName HiFunUnpackBytes = "unpack-bytes"
funName HiFunEncodeUtf8 = "encode-utf8"
funName HiFunDecodeUtf8 = "decode-utf8"
funName HiFunZip = "zip"
funName HiFunUnzip = "unzip"
funName HiFunSerialise = "serialise"
funName HiFunDeserialise = "deserialise"
