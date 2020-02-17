{-# LANGUAGE LambdaCase #-}

import Parser

import System.Environment

main = do
  args <- getArgs
  case args of
    ["sed"] -> sed

sed :: IO ()
sed = do
  input <- getLine
  case input of
    _ -> print "placeholder"

