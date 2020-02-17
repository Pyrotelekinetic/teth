{-# LANGUAGE LambdaCase #-}

import System.Environment

main = do
  args <- getArgs
  case args of
    _ -> putStrLn "placeholder"

