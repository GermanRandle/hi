module HW5.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as B
import qualified Data.Sequence as S
import Data.Set (Set, member)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock as C
import HW5.Base (HiAction (..), HiMonad, HiValue (..), runAction)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException =
  PermissionRequired HiPermission
  deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance HiMonad HIO where
  runAction HiActionCwd = HIO $ \perm -> do
    checkPerm perm AllowRead
    HiValueString . T.pack <$> getCurrentDirectory
  runAction (HiActionChDir dirPath) = HIO $ \perm -> do
    checkPerm perm AllowRead
    HiValueNull <$ setCurrentDirectory dirPath
  runAction (HiActionRead path) = HIO $ \perm -> do
    checkPerm perm AllowRead
    isFile <- doesFileExist path
    if isFile
      then do
        content <- B.readFile path
        return $ case decodeUtf8' content of
          (Left _) -> HiValueBytes content
          (Right str) -> HiValueString str
      else do
        content <- listDirectory path
        return $ HiValueList $ S.fromList $ map (HiValueString . T.pack) content
  runAction (HiActionWrite path content) = HIO $ \perm -> do
    checkPerm perm AllowWrite
    HiValueNull <$ B.writeFile path content
  runAction (HiActionMkDir path) = HIO $ \perm -> do
    checkPerm perm AllowWrite
    HiValueNull <$ createDirectory path
  runAction HiActionNow = HIO $ \perm -> do
    checkPerm perm AllowTime
    HiValueTime <$> C.getCurrentTime
  runAction (HiActionRand l r) = HIO $ const (do HiValueNumber . toRational <$> getStdRandom (uniformR (l, r)))
  runAction (HiActionEcho t) = HIO $ \perm -> do
    checkPerm perm AllowWrite
    HiValueNull <$ putStrLn (T.unpack t)

instance Monad HIO where
  m >>= f = HIO $ \perm -> runHIO m perm >>= \res -> runHIO (f res) perm

instance Applicative HIO where
  pure val = HIO $ const (do return val)
  ma <*> mb = HIO $ \perm -> runHIO ma perm <*> runHIO mb perm

instance Functor HIO where
  fmap f m = HIO $ \perm -> fmap f (runHIO m perm)

checkPerm :: Set HiPermission -> HiPermission -> IO ()
checkPerm actual required = do
  if member required actual
  then return ()
  else throwIO $ PermissionRequired required 
