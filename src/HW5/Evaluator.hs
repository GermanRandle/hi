{-# LANGUAGE TupleSections #-}

module HW5.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Except (ExceptT, foldM, lift, throwError)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Either (fromRight)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock as C
import Data.Word (Word8)
import HW5.Base (HiAction (..), HiError (..), HiExpr (..), HiFun(..), HiMonad, HiValue (..), runAction)
import Text.Read (readMaybe)

type ArgTaker m a = HiValue -> Evaluator m a
type BinaryFunction a b c = a -> b -> c
type Evaluator = ExceptT HiError
type UnaryFunction a b = a -> b

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval expr = runExceptT $ eval' expr

eval' :: HiMonad m => HiExpr -> Evaluator m HiValue
eval' (HiExprValue x) = return x
eval' (HiExprApply f args) = evalApply f args
eval' (HiExprRun r) = evalRun r
eval' (HiExprDict d) = evalDict d

evalApply :: HiMonad m => HiExpr -> [HiExpr] -> Evaluator m HiValue
evalApply object args = do
  eObject <- eval' object
  case eObject of
    (HiValueFunction f) -> evalFunc f args
    _ -> do
      eArgs <- mapM eval' args
      case eObject of
        (HiValueString s) -> stringIndex s eArgs
        (HiValueList l) -> listIndex l eArgs
        (HiValueBytes b) -> bytesIndex b eArgs
        (HiValueDict d) -> dictGet d eArgs
        _ -> throwError HiErrorInvalidFunction

evalDict :: HiMonad m => [(HiExpr, HiExpr)] -> Evaluator m HiValue
evalDict l = do
  el <- mapM (\(a, b) -> do
    ea <- eval' a
    eb <- eval' b
    return (ea, eb)) l
  return $ HiValueDict $ M.fromList el

evalRun :: HiMonad m => HiExpr -> Evaluator m HiValue
evalRun expr = do
  runnable <- eval' expr
  case runnable of
    (HiValueAction action) -> lift $ runAction action
    _ -> throwError HiErrorInvalidArgument

evalFunc :: HiMonad m => HiFun -> [HiExpr] -> Evaluator m HiValue
evalFunc HiFunIf [cond, thenBranch, elseBranch] = do
  eCond <- eval' cond >>= takeBool
  if eCond then eval' thenBranch else eval' elseBranch
evalFunc HiFunIf _ = throwError HiErrorArityMismatch
evalFunc HiFunAnd [a, b] = do
  ea <- eval' a
  case ea of
    (HiValueBool False) -> return $ HiValueBool False
    HiValueNull -> return HiValueNull
    _ -> eval' b
evalFunc HiFunOr [a, b] = do
  ea <- eval' a
  case ea of
    (HiValueBool False) -> eval' b
    HiValueNull -> eval' b
    res -> return res
evalFunc f args = do
  eArgs <- mapM eval' args
  evalFuncNotLazy f eArgs

evalFuncNotLazy :: HiMonad m => HiFun -> [HiValue] -> Evaluator m HiValue
evalFuncNotLazy HiFunList l = return $ HiValueList $ S.fromList l
evalFuncNotLazy HiFunFold [f, l] = do
  ef <- takeFun f
  el <- takeList l
  case el of
    S.Empty -> return HiValueNull
    (h S.:<| t) -> foldM (evalFuncBinary ef) h t
evalFuncNotLazy HiFunFold _ = throwError HiErrorArityMismatch
evalFuncNotLazy f [a] = evalFuncUnary f a
evalFuncNotLazy f [a, b] = evalFuncBinary f a b
evalFuncNotLazy _ _ = throwError HiErrorArityMismatch

evalFuncUnary, evalFuncUnaryPolymorphic :: HiMonad m => HiFun -> HiValue -> Evaluator m HiValue
evalFuncUnary HiFunNot = evalFuncUnary' takeBool not HiValueBool
evalFuncUnary HiFunToUpper = evalFuncUnary' takeText T.toUpper HiValueString
evalFuncUnary HiFunToLower = evalFuncUnary' takeText T.toLower HiValueString
evalFuncUnary HiFunTrim = evalFuncUnary' takeText T.strip HiValueString
evalFuncUnary HiFunPackBytes = evalFuncUnary' takeWord8List B.pack HiValueBytes
evalFuncUnary HiFunUnpackBytes = evalFuncUnary' takeBinary (map (HiValueNumber . toRational) . B.unpack) (HiValueList . S.fromList)
evalFuncUnary HiFunEncodeUtf8 = evalFuncUnary' takeText encodeUtf8 HiValueBytes
evalFuncUnary HiFunDecodeUtf8 = evalFuncUnary' takeBinary (either (const HiValueNull) HiValueString . decodeUtf8') id
evalFuncUnary HiFunZip = evalFuncUnary' takeBinary (toStrict . compressWith defaultCompressParams { compressLevel = bestCompression } . fromStrict) HiValueBytes
evalFuncUnary HiFunUnzip = evalFuncUnary' takeBinary (toStrict . decompress . fromStrict) HiValueBytes
evalFuncUnary HiFunSerialise = evalFuncUnary' return (toStrict . serialise) HiValueBytes
evalFuncUnary HiFunDeserialise = evalFuncUnary' takeBinary (fromRight HiValueNull . (deserialiseOrFail . fromStrict)) id
evalFuncUnary HiFunRead = evalFuncUnary' takeText T.unpack (HiValueAction . HiActionRead)
evalFuncUnary HiFunMkDir = evalFuncUnary' takeText T.unpack (HiValueAction . HiActionMkDir)
evalFuncUnary HiFunChDir = evalFuncUnary' takeText T.unpack (HiValueAction . HiActionChDir)
evalFuncUnary HiFunParseTime = evalFuncUnary' takeText (maybe HiValueNull HiValueTime . (readMaybe . T.unpack)) id
evalFuncUnary HiFunEcho = evalFuncUnary' takeText HiActionEcho HiValueAction
evalFuncUnary HiFunKeys = evalFuncUnary' takeDict M.keys (HiValueList . S.fromList)
evalFuncUnary HiFunValues = evalFuncUnary' takeDict M.elems (HiValueList . S.fromList)
evalFuncUnary HiFunInvert = evalFuncUnary' takeDict dictInvert HiValueDict
evalFuncUnary f = evalFuncUnaryPolymorphic f

evalFuncUnaryPolymorphic HiFunLength s@(HiValueString _) = evalFuncUnary' takeText (toRational . T.length) HiValueNumber s
evalFuncUnaryPolymorphic HiFunLength l = evalFuncUnary' takeList (toRational . S.length) HiValueNumber l
evalFuncUnaryPolymorphic HiFunReverse s@(HiValueString _) = evalFuncUnary' takeText T.reverse HiValueString s
evalFuncUnaryPolymorphic HiFunReverse l = evalFuncUnary' takeList S.reverse HiValueList l
evalFuncUnaryPolymorphic HiFunCount s@(HiValueString _) = evalFuncUnary' takeText (countOccurs (HiValueString . T.singleton) T.unpack) HiValueDict s
evalFuncUnaryPolymorphic HiFunCount b@(HiValueBytes _) = evalFuncUnary' takeBinary (countOccurs (HiValueNumber . toRational) B.unpack) HiValueDict b
evalFuncUnaryPolymorphic HiFunCount l = evalFuncUnary' takeList (countOccurs id toList) HiValueDict l
evalFuncUnaryPolymorphic _ _ = throwError HiErrorArityMismatch

evalFuncUnary' :: HiMonad m => ArgTaker m a -> UnaryFunction a b -> (b -> HiValue) -> HiValue -> Evaluator m HiValue
evalFuncUnary' argTaker f resWrapper a = do
  ea <- argTaker a
  return $ resWrapper $ f ea

evalFuncBinary, evalFuncBinaryPolymorphic :: HiMonad m => HiFun -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary HiFunLessThan = evalFuncBinary' return return  (<) HiValueBool
evalFuncBinary HiFunGreaterThan = evalFuncBinary' return return (>) HiValueBool
evalFuncBinary HiFunEquals = evalFuncBinary' return return (==) HiValueBool
evalFuncBinary HiFunNotLessThan = evalFuncBinary' return return (>=) HiValueBool
evalFuncBinary HiFunNotGreaterThan = evalFuncBinary' return return (<=) HiValueBool
evalFuncBinary HiFunNotEquals = evalFuncBinary' return return (/=) HiValueBool
evalFuncBinary HiFunRange = evalFuncBinary' takeNum takeNum (\a b -> [a .. b]) (HiValueList . S.fromList . map HiValueNumber)
evalFuncBinary HiFunWrite = evalFuncBinary' takeText takeText (\f t -> HiValueAction $ HiActionWrite (T.unpack f) (encodeUtf8 t)) id
evalFuncBinary HiFunRand = evalFuncBinary' takeInteger takeInteger (\l r -> HiActionRand (fromInteger l) (fromInteger r)) HiValueAction
evalFuncBinary f = evalFuncBinaryPolymorphic f

evalFuncBinaryPolymorphic HiFunAdd a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (+) HiValueNumber a
evalFuncBinaryPolymorphic HiFunAdd a@(HiValueString _) = evalFuncBinary' takeText takeText (<>) HiValueString a
evalFuncBinaryPolymorphic HiFunAdd a@(HiValueBytes _) = evalFuncBinary' takeBinary takeBinary B.append HiValueBytes a
evalFuncBinaryPolymorphic HiFunAdd a@(HiValueTime _) = evalFuncBinary' takeTime takeNatural (\t diff -> C.addUTCTime (fromInteger diff) t) HiValueTime a
evalFuncBinaryPolymorphic HiFunAdd a = evalFuncBinary' takeList takeList (S.><) HiValueList a
evalFuncBinaryPolymorphic HiFunDiv a@(HiValueNumber _) = evalFuncBinary' takeNum takeDivisor (/) HiValueNumber a
evalFuncBinaryPolymorphic HiFunDiv a = evalFuncBinary' takeText takeText (\ x y -> T.snoc x '/' <> y) HiValueString a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (*) HiValueNumber a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueString _) = evalFuncBinary' takeText takeNatural (flip stimes) HiValueString a
evalFuncBinaryPolymorphic HiFunMul a@(HiValueBytes _) = evalFuncBinary' takeBinary takeNatural (flip stimes) HiValueBytes a
evalFuncBinaryPolymorphic HiFunMul a = evalFuncBinary' takeList takeNatural (flip stimes) HiValueList a
evalFuncBinaryPolymorphic HiFunSub a@(HiValueNumber _) = evalFuncBinary' takeNum takeNum (-) HiValueNumber a
evalFuncBinaryPolymorphic HiFunSub a = evalFuncBinary' takeTime takeTime (\x y -> toRational (C.diffUTCTime x y)) HiValueNumber a
evalFuncBinaryPolymorphic _ _ = do const (throwError HiErrorArityMismatch)

evalFuncBinary' :: HiMonad m => ArgTaker m a -> ArgTaker m b -> BinaryFunction a b c -> (c -> HiValue) -> HiValue -> HiValue -> Evaluator m HiValue
evalFuncBinary' argTaker1 argTaker2 f resWrapper a b = do
  ea <- argTaker1 a
  eb <- argTaker2 b
  return $ resWrapper $ f ea eb

stringIndex :: HiMonad m => T.Text -> [HiValue] -> Evaluator m HiValue
stringIndex s [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ if idx >= 0 && T.compareLength s idx == GT
           then HiValueString $ T.singleton $ s `T.index` idx
           else HiValueNull
stringIndex s [el1, el2] = do
  res <- slice (T.unpack s) el1 el2
  return $ HiValueString $ T.pack res
stringIndex _ _ = throwError HiErrorArityMismatch

listIndex :: HiMonad m => S.Seq HiValue -> [HiValue] -> Evaluator m HiValue
listIndex l [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ fromMaybe HiValueNull (l S.!? idx)
listIndex l [el1, el2] = do
  res <- slice (toList l) el1 el2
  return $ HiValueList $ S.fromList res
listIndex _ _ = throwError HiErrorArityMismatch

bytesIndex :: HiMonad m => B.ByteString -> [HiValue] -> Evaluator m HiValue
bytesIndex b [el] = do
  idx <- fromIntegral <$> takeInteger el
  return $ maybe HiValueNull (HiValueNumber . toRational . fst)  (B.uncons $ B.drop idx b) -- in bytestring-0.11.0, there is B.!? with O(1) complexity :(
bytesIndex b [el1, el2] = do
  res <- slice (B.unpack b) el1 el2
  return $ HiValueBytes $ B.pack res
bytesIndex _ _ = throwError HiErrorArityMismatch

slice :: HiMonad m => [a] -> HiValue -> HiValue -> Evaluator m [a]
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

-- Auxiliary

dictGet :: HiMonad m => M.Map HiValue HiValue -> [HiValue] -> Evaluator m HiValue
dictGet m [k] = do
  return $ case M.lookup k m of
    (Just val) -> val
    Nothing -> HiValueNull
dictGet _ _ = throwError HiErrorArityMismatch

dictInvert :: M.Map HiValue HiValue -> M.Map HiValue HiValue
dictInvert m = M.map (HiValueList . S.fromList) (M.fromListWith (++) (map (\(k, v) -> (v, [k])) (M.toList m)))

countOccurs :: Ord b => (b -> HiValue) -> (a -> [b]) -> a -> M.Map HiValue HiValue
countOccurs keyMapper unpacker l = 
  let cnt = M.fromListWith (+) (map (, 1 :: Integer) (unpacker l))
  in  M.map (HiValueNumber . toRational) (M.mapKeys keyMapper cnt)

-- ArgTakers

takeNum, takeDivisor :: HiMonad m => ArgTaker m Rational
takeNum (HiValueNumber x) = return x
takeNum _ = throwError HiErrorInvalidArgument

takeDivisor val = do
  d <- takeNum val
  if d == 0 then throwError HiErrorDivideByZero else return d

takeBool :: HiMonad m => ArgTaker m Bool
takeBool (HiValueBool b) = return b
takeBool _ = throwError HiErrorInvalidArgument

takeText :: HiMonad m => ArgTaker m T.Text
takeText (HiValueString t) = return t
takeText _ = throwError HiErrorInvalidArgument

takeInteger, takeNatural :: HiMonad m => ArgTaker m Integer
takeInteger val = do
  x <- takeNum val
  if denominator x == 1 then return $ numerator x else throwError HiErrorInvalidArgument

takeNatural val = do
  x <- takeInteger val
  if x >= 0 then return x else throwError HiErrorInvalidArgument

takeFun :: HiMonad m => ArgTaker m HiFun
takeFun (HiValueFunction f) = return f
takeFun _ = throwError HiErrorInvalidArgument

takeList :: HiMonad m => ArgTaker m (S.Seq HiValue)
takeList (HiValueList l) = return l
takeList _ = throwError HiErrorInvalidArgument

takeWord8List :: HiMonad m => ArgTaker m [Word8]
takeWord8List val = do
  l <- takeList val
  mapM validate (toList l) where
    validate :: HiMonad m => ArgTaker m Word8
    validate (HiValueNumber x) = 
      if x >= 0 && x <= 255 && denominator x == 1 
      then return $ fromInteger $ numerator x
      else throwError HiErrorInvalidArgument
    validate _ = throwError HiErrorInvalidArgument

takeBinary :: HiMonad m => ArgTaker m B.ByteString
takeBinary (HiValueBytes b) = return b
takeBinary _ = throwError HiErrorInvalidArgument

takeTime :: HiMonad m => ArgTaker m C.UTCTime
takeTime (HiValueTime t) = return t
takeTime _ = throwError HiErrorInvalidArgument

takeDict :: HiMonad m => ArgTaker m (M.Map HiValue HiValue)
takeDict (HiValueDict d) = return d
takeDict _ = throwError HiErrorInvalidArgument
