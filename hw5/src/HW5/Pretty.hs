module HW5.Pretty
  ( prettyValue
  ) where

import Data.Ratio ((%), denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import HW5.Base (HiValue (..))
import Prettyprinter (Doc, (<+>), pretty, slash)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num) = prettyValueNumber num
prettyValue _ = undefined

prettyValueNumber :: Rational -> Doc AnsiStyle
prettyValueNumber x = prettyValueNumber' (numerator x) (denominator x)

prettyValueNumber' :: Integer -> Integer -> Doc AnsiStyle
prettyValueNumber' n 1 = pretty n
prettyValueNumber' n d = prettyValueFloat n d

prettyValueFloat :: Integer -> Integer -> Doc AnsiStyle
prettyValueFloat n d = if recDivBy2And5 d == 1
                       then let f = aux n d
                            in pretty $ show f
                       else prettyValueFraction n d

aux :: Integer -> Integer -> Double
aux n d = toRealFloat $ fst $ fromRationalRepetendUnlimited (n % d)

prettyValueFraction :: Integer -> Integer -> Doc AnsiStyle
prettyValueFraction n d = if abs n < d
                          then pretty n <> slash <> pretty d
                          else prettyValueFractionMixed n d 

prettyValueFractionMixed :: Integer -> Integer -> Doc AnsiStyle
prettyValueFractionMixed n d = pretty (quot n d) <+> pretty (if n < 0 then "-" else "+") <+> prettyValueFraction (abs $ rem n d) d

recDivBy2And5 :: Integer -> Integer
recDivBy2And5 x
  | even x = recDivBy2And5 $ x `div` 2
  | x `mod` 5 == 0 = recDivBy2And5 $ x `div` 5
  | otherwise = x
