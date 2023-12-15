module Main (main) where

import HW5.Parser (parse)
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
            (Right expr) -> outputStrLn $ "Parse success: " ++ show expr
          loop
