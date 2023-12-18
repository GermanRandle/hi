module HW5.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.Ratio ((%), denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import qualified Data.Sequence as S
import Data.Time.Clock as C
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiValue (..), funName)
import Numeric (showHex)
import Prettyprinter (Doc, (<+>), comma, dquotes, encloseSep, lbracket, lparen, pretty, rbracket, rparen, slash, space, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x) = prettyValueNumber (numerator x) (denominator x)
prettyValue (HiValueFunction f) = pretty $ funName f
prettyValue (HiValueBool b) = pretty $ map toLower (show b)
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString s) = viaShow s
prettyValue (HiValueList l) = prettyValueList l
prettyValue (HiValueBytes b) = prettyValueBytes b
prettyValue (HiValueAction a) = prettyValueAction a
prettyValue (HiValueTime t) = prettyValueTime t

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

prettyValueList :: S.Seq HiValue -> Doc AnsiStyle
prettyValueList S.Empty = pretty "[]"
prettyValueList l = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map prettyValue (toList l))

-- special case for empty? e.g. if empty B.unpack b then ...
prettyValueBytes :: B.ByteString -> Doc AnsiStyle
prettyValueBytes b = encloseSep (lbracket <> grid <> space) (space <> grid <> rbracket) space (map prettyHex (B.unpack b)) where
  grid :: Doc AnsiStyle
  grid = pretty "#"

  prettyHex :: Word8 -> Doc AnsiStyle
  prettyHex w = pretty $ (if w < 16 then "0" else "") ++ showHex w ""

prettyValueAction :: HiAction -> Doc AnsiStyle
prettyValueAction (HiActionRead f) = pretty "read" <> lparen <> viaShow f <> rparen
prettyValueAction (HiActionWrite f t) = pretty "write" <> lparen <> viaShow f <> comma <+> prettyValueBytes t <> rparen
prettyValueAction (HiActionMkDir d) = pretty "mkdir" <> lparen <> viaShow d <> rparen
prettyValueAction (HiActionChDir d) = pretty "cd" <> lparen <> viaShow d <> rparen
prettyValueAction HiActionCwd = pretty "cwd"
prettyValueAction HiActionNow = pretty "now"
prettyValueAction (HiActionRand l r) = pretty "rand" <> lparen <+> viaShow l <> comma <+> viaShow r <+> rparen

prettyValueTime :: C.UTCTime -> Doc AnsiStyle
prettyValueTime t = pretty "parse-time" <> lparen <> dquotes (viaShow t) <> rparen
