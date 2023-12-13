module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import HW5.Base (HiExpr (..), HiValue (..), HiFun (..))
import Text.Megaparsec.Error (ParseErrorBundle)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse _ = Right $ HiExprValue $ HiValueFunction HiFunDiv
