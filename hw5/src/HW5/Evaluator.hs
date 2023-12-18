module HW5.Evaluator
  ( eval
  ) where

import Control.Monad.Except (ExceptT, foldM, throwError)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import HW5.Base (HiError (..), HiExpr (..), HiFun(..), HiValue (..))

type ArgTaker m a = HiValue -> Evaluator m a
type BinaryFunction a b c = a -> b -> c
type Evaluator = ExceptT HiError
type UnaryFunction a b = a -> b

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ eval' expr

eval' :: Monad m => HiExpr -> Evaluator m HiValue
eval' (HiExprValue x) = return x
eval' (HiExprApply f args) = evalApply f args

evalApply :: Monad m => HiExpr -> [HiExpr] -> Evaluator m HiValue
evalApply object args = do
  eObject <- eval' object
  eArgs <- mapM eval' args
  case eObject of
    (HiValueFunction f) -> evalFunc f eArgs
    (HiValueString s) -> stringIndex s eArgs
    (HiValueList l) -> listIndex l eArgs
    (HiValueBytes b) -> bytesIndex b eArgs
    _ -> throwError HiErrorInvalidFunction

evalFunc :: Monad m => HiFun -> [HiValue] -> Evaluator m HiValue
evalFunc HiFunList l = return $ HiValueList $ S.fromList l
evalFunc HiFunFold args = do
  case args of
    [f, l] -> do
      ef <- takeFun f
      el <- takeList l
      case el of
        S.Empty -> return $ HiValueList S.Empty
        (h S.:<| t) -> foldM (evalFuncBinary ef) h t
    _ -> throwError HiErrorArityMismatch
evalFunc f [a] = evalFuncUnary f a
evalFunc f [a, b] = evalFuncBinary f a b
evalFunc f [a, b, c] = evalFuncTernary f a b c
evalFunc _ _ = throwError HiErrorArityMismatch

evalFuncUnary, evalFuncUnaryPolymorphic :: Monad m => HiFun -> HiValue -> Evaluator m HiValue
evalFuncUnary HiFunNot = evalFuncUnary' takeBool not HiValueBool
evalFuncUnary HiFunToUpper = evalFuncUnary' takeText T.toUpper HiValueString
evalFuncUnary HiFunToLower = evalFuncUnary' takeText T.toLower HiValueString
evalFuncUnary HiFunTrim = evalFuncUnary' takeText T.strip HiValueString
evalFuncUnary HiFunPackBytes = evalFuncUnary' takeWord8List B.pack HiValueBytes
evalFuncUnary HiFunUnpackBytes = evalFuncUnary' takeBinary (S.fromList . map (HiValueNumber . toRational) . B.unpack) HiValueList
evalFuncUnary HiFunEncodeUtf8 = evalFuncUnary' takeText encodeUtf8 HiValueBytes
evalFuncUnary HiFunDecodeUtf8 = evalFuncUnary' takeBinary (either (const HiValueNull) HiValueString . decodeUtf8') id
evalFuncUnary f = evalFuncUnaryPolymorphic f

evalFuncUnaryPolymorphic HiFunLength s@(HiValueString _) = evalFuncUnary' takeText (toRational . T.length) HiValueNumber s
evalFuncUnaryPolymorphic HiFunLength l = evalFuncUnary' takeList (toRational . S.length) HiValueNumber l
evalFuncUnaryPolymorphic HiFunReverse s@(HiValueString _) = evalFuncUnary' takeText T.reverse HiValueString s
evalFuncUnaryPolymorphic HiFunReverse l = evalFuncUnary' takeList S.reverse HiValueList l
evalFuncUnaryPolymorphic _ _ = throwError HiErrorArityMismatch

evalFuncUnary' :: Monad m => ArgTaker m a -> UnaryFunction a b -> (b -> HiValue) -> HiValue -> Evaluator m HiValue
evalFuncUnary' argTaker f resWrapper a = do
  ea <- argTaker a
  return $ resWrapper $ f ea

evalFuncBinary, evalFuncBinaryPolymorphic :: Monad m => HiFun -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary HiFunSub = evalFuncBinary' takeNum takeNum (-) HiValueNumber
evalFuncBinary HiFunAnd = evalFuncBinary' takeBool takeBool (&&) HiValueBool
evalFuncBinary HiFunOr = evalFuncBinary' takeBool takeBool (||) HiValueBool
evalFuncBinary HiFunLessThan = evalFuncBinary' return return  (<) HiValueBool
evalFuncBinary HiFunGreaterThan = evalFuncBinary' return return (>) HiValueBool
evalFuncBinary HiFunEquals = evalFuncBinary' return return (==) HiValueBool
evalFuncBinary HiFunNotLessThan = evalFuncBinary' return return (>=) HiValueBool
evalFuncBinary HiFunNotGreaterThan = evalFuncBinary' return return (<=) HiValueBool
evalFuncBinary HiFunNotEquals = evalFuncBinary' return return (/=) HiValueBool
evalFuncBinary HiFunRange = evalFuncBinary' takeNum takeNum (\a b -> [a .. b]) (HiValueList . S.fromList . map HiValueNumber)
evalFuncBinary f = evalFuncBinaryPolymorphic f

evalFuncBinaryPolymorphic HiFunAdd a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (+) HiValueNumber a
evalFuncBinaryPolymorphic HiFunAdd a@(HiValueString _) = evalFuncBinary' takeText takeText (<>) HiValueString a
evalFuncBinaryPolymorphic HiFunAdd a@(HiValueBytes _) = evalFuncBinary' takeBinary takeBinary B.append HiValueBytes a
evalFuncBinaryPolymorphic HiFunAdd a = evalFuncBinary' takeList takeList (S.><) HiValueList a
evalFuncBinaryPolymorphic HiFunDiv a@(HiValueNumber _) = evalFuncBinary' takeNum takeDivisor (/) HiValueNumber a
evalFuncBinaryPolymorphic HiFunDiv a = evalFuncBinary' takeText takeText (\ x y -> T.snoc x '/' <> y) HiValueString a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (*) HiValueNumber a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueString _) = evalFuncBinary' takeText takeNatural (flip stimes) HiValueString a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueBytes _) = evalFuncBinary' takeBinary takeNatural (flip stimes) HiValueBytes a
evalFuncBinaryPolymorphic HiFunMul a = evalFuncBinary' takeList takeNatural (flip stimes) HiValueList a
evalFuncBinaryPolymorphic _ _ = do const (throwError HiErrorArityMismatch)

evalFuncBinary' :: Monad m => ArgTaker m a -> ArgTaker m b -> BinaryFunction a b c -> (c -> HiValue) -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary' argTaker1 argTaker2 f resWrapper a b = do
  ea <- argTaker1 a
  eb <- argTaker2 b
  return $ resWrapper $ f ea eb

evalFuncTernary :: Monad m => HiFun -> HiValue -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncTernary HiFunIf cond thenBranch elseBranch = do
  eCond <- takeBool cond
  return $ if eCond then thenBranch else elseBranch
evalFuncTernary _ _ _ _ = throwError HiErrorArityMismatch

stringIndex :: Monad m => T.Text -> [HiValue] -> Evaluator m HiValue
stringIndex s [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ if idx >= 0 && T.compareLength s idx == GT
           then HiValueString $ T.singleton $ s `T.index` idx
           else HiValueNull
stringIndex s [el1, el2] = do
  res <- slice (T.unpack s) el1 el2
  return $ HiValueString $ T.pack res
stringIndex _ _ = throwError HiErrorArityMismatch

listIndex :: Monad m => S.Seq HiValue -> [HiValue] -> Evaluator m HiValue
listIndex l [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ fromMaybe HiValueNull (l S.!? idx)
listIndex l [el1, el2] = do
  res <- slice (toList l) el1 el2
  return $ HiValueList $ S.fromList res
listIndex _ _ = throwError HiErrorArityMismatch

bytesIndex :: Monad m => B.ByteString -> [HiValue] -> Evaluator m HiValue
bytesIndex b [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ maybe HiValueNull (HiValueNumber . toRational . fst)  (B.uncons $ B.drop idx b)
bytesIndex b [el1, el2] = do
  res <- slice (B.unpack b) el1 el2
  return $ HiValueBytes $ B.pack res
bytesIndex _ _ = throwError HiErrorArityMismatch

slice :: Monad m => [a] -> HiValue -> HiValue -> Evaluator m [a]
slice lst HiValueNull r@(HiValueNumber _) = slice lst (HiValueNumber 0) r
slice lst l@(HiValueNumber _) HiValueNull = slice lst l (HiValueNumber (toRational $ length lst))
slice lst l@(HiValueNumber _) r@(HiValueNumber _) = do
  l' <- normalize . fromIntegral <$> takeInteger l
  r' <- normalize . fromIntegral <$> takeInteger r
  return $ take (r' - l') (drop l' lst)
  where
    normalize :: Int -> Int
    normalize idx = if idx < 0 then idx + length lst else idx
slice _ _ _ = throwError HiErrorInvalidArgument

-- ArgTakers

takeNum, takeDivisor :: Monad m => ArgTaker m Rational
takeNum (HiValueNumber x) = return x
takeNum _ = throwError HiErrorInvalidArgument

takeDivisor val = do
  d <- takeNum val
  if d == 0 then throwError HiErrorDivideByZero else return d

takeBool :: Monad m => ArgTaker m Bool
takeBool (HiValueBool b) = return b
takeBool _ = throwError HiErrorInvalidArgument

takeText :: Monad m => ArgTaker m T.Text
takeText (HiValueString t) = return t
takeText _ = throwError HiErrorInvalidArgument

takeInteger, takeNatural :: Monad m => ArgTaker m Integer
takeInteger val = do
  x <- takeNum val
  if denominator x == 1 then return $ numerator x else throwError HiErrorInvalidArgument

takeNatural val = do
  x <- takeInteger val
  if x >= 0 then return x else throwError HiErrorInvalidArgument

takeFun :: Monad m => ArgTaker m HiFun
takeFun (HiValueFunction f) = return f
takeFun _ = throwError HiErrorInvalidArgument

takeList :: Monad m => ArgTaker m (S.Seq HiValue)
takeList (HiValueList l) = return l
takeList _ = throwError HiErrorInvalidArgument

takeWord8List :: Monad m => ArgTaker m [Word8]
takeWord8List val = do
  l <- takeList val
  mapM validate (toList l) where
    validate :: Monad m => ArgTaker m Word8
    validate (HiValueNumber x) = 
      if x >= 0 && x <= 255 && denominator x == 1 
      then return $ fromInteger $ numerator x
      else throwError HiErrorInvalidArgument
    validate _ = throwError HiErrorInvalidArgument

takeBinary :: Monad m => ArgTaker m B.ByteString
takeBinary (HiValueBytes b) = return b
takeBinary _ = throwError HiErrorInvalidArgument
