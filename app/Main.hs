module Main where

import Text.Parsec.Error
import Parse

main :: IO ()
main = do 
  inp <- getContents
  case parseCoqToMd inp of
    Left  err -> print err
    Right str -> putStrLn str
