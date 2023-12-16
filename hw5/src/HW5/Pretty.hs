module HW5.Pretty
  ( prettyValue
  ) where

import Data.Ratio ((%), denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import HW5.Base (HiValue (..), funName)
import Prettyprinter (Doc, (<+>), pretty, slash)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x) = prettyValueNumber (numerator x) (denominator x)
prettyValue (HiValueFunction f) = pretty $ funName f

prettyValueNumber :: Integer -> Integer -> Doc AnsiStyle
prettyValueNumber n d
  | d == 1 = pretty n
  | recDivBy2And5 d == 1 = pretty $ show $ fracToDouble n d
  | abs n < d = prettyValueFraction n d
  | otherwise = pretty (quot n d)
            <+> pretty (if n < 0 then "-" else "+")
            <+> prettyValueFraction (abs $ rem n d) d
  where
    fracToDouble :: Integer -> Integer -> Double
    fracToDouble n' d' = toRealFloat $ fst $ fromRationalRepetendUnlimited (n' % d')

    prettyValueFraction :: Integer -> Integer -> Doc AnsiStyle
    prettyValueFraction n' d' = pretty n' <> slash <> pretty d'

    recDivBy2And5 :: Integer -> Integer
    recDivBy2And5 x
      | even x = recDivBy2And5 $ x `div` 2
      | x `mod` 5 == 0 = recDivBy2And5 $ x `div` 5
      | otherwise = x
