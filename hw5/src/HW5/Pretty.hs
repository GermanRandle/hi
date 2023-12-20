module HW5.Pretty
  ( prettyValue
  ) where

import qualified Data.ByteString as B
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Ratio ((%), denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import qualified Data.Sequence as S
import Data.Time.Clock as C
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiValue (..), funName)
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber x) = prettyNumber (numerator x) (denominator x)
prettyValue (HiValueList l) = prettySeq l
prettyValue (HiValueBytes b) = prettyBytes b
prettyValue (HiValueAction a) = prettyAction a
prettyValue (HiValueTime t) = prettyTime t
prettyValue (HiValueDict d) = prettyDict d
prettyValue (HiValueString s) = viaShow s
prettyValue (HiValueFunction f) = pretty $ funName f
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue HiValueNull = pretty "null"

prettyNumber :: Integer -> Integer -> Doc AnsiStyle
prettyNumber n d
  | d == 1 = pretty n
  | recDivBy2And5 d == 1 = pretty $ show fracToDouble
  | abs n < d = prettyFraction n d
  | otherwise = pretty (quot n d)
            <+> pretty (if n < 0 then "-" else "+")
            <+> prettyFraction (abs $ rem n d) d
  where
    fracToDouble :: Double
    fracToDouble = toRealFloat $ fst $ fromRationalRepetendUnlimited (n % d)

    prettyFraction :: Integer -> Integer -> Doc AnsiStyle
    prettyFraction n' d' = pretty n' <> slash <> pretty d'

    recDivBy2And5 :: Integer -> Integer
    recDivBy2And5 x
      | even x = recDivBy2And5 $ x `div` 2
      | x `mod` 5 == 0 = recDivBy2And5 $ x `div` 5
      | otherwise = x

prettySeq :: S.Seq HiValue -> Doc AnsiStyle
prettySeq S.Empty = pretty "[]"
prettySeq (x S.:<| S.Empty) = pretty "[" <> prettyValue x <> pretty "]"
prettySeq l = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map prettyValue (toList l))

prettyBytes :: B.ByteString -> Doc AnsiStyle
prettyBytes b = encloseSep (lbracket <> grid <> space) (space <> grid <> rbracket) space (map prettyHex (B.unpack b))
  where
    grid :: Doc AnsiStyle
    grid = pretty "#"

    prettyHex :: Word8 -> Doc AnsiStyle
    prettyHex w = pretty $ (if w < 16 then "0" else "") ++ showHex w ""

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead f) = pretty "read" <> parens (viaShow f)
prettyAction (HiActionWrite f t) = pretty "write" <> parens (viaShow f <> comma <+> prettyBytes t)
prettyAction (HiActionMkDir d) = pretty "mkdir" <> parens (viaShow d)
prettyAction (HiActionChDir d) = pretty "cd" <> parens (viaShow d)
prettyAction (HiActionRand l r) = pretty "rand" <> lparen <+> viaShow l <> comma <+> viaShow r <+> rparen
prettyAction (HiActionEcho t) = pretty "echo" <> parens (viaShow t)
prettyAction HiActionCwd = pretty "cwd"
prettyAction HiActionNow = pretty "now"

prettyTime :: C.UTCTime -> Doc AnsiStyle
prettyTime t = pretty "parse-time" <> parens (dquotes (viaShow t))

prettyDict :: M.Map HiValue HiValue -> Doc AnsiStyle
prettyDict d = 
  if null d
  then pretty "{}"
  else encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) (map prettyEntry (M.assocs d))
    where
      prettyEntry :: (HiValue, HiValue) -> Doc AnsiStyle
      prettyEntry (k, v) = prettyValue k <> colon <> space <> prettyValue v
