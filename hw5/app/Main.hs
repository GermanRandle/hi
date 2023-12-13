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
        Just "q" -> return ()
        Just input -> do
          outputStrLn $ show $ parse input
          loop
