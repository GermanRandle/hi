module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Set (Set, fromList)
import HW5.Action (HiPermission (..), runHIO)
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "" -> return ()
        Just input -> do
          case parse input of
            (Left parseErr) -> outputStrLn $ "Parse error: " ++ show parseErr
            (Right expr) -> do
              outputStrLn $ "Parse success: " ++ show expr
              evaluated <- liftIO $ runHIO (eval expr) permissons
              case evaluated of
                (Left evalErr) -> outputStrLn $ "Eval error: " ++ show evalErr
                (Right value) -> do
                  outputStrLn $ "Eval success: " ++ show value
                  outputStrLn $ "Pretty: " ++ show (prettyValue value)
          loop

    permissons :: Set HiPermission
    permissons = fromList [AllowRead, AllowWrite, AllowTime]
