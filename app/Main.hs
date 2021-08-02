module Main where

import Data.List
import Data.Time.Clock
import Data.Time.Calendar


import System.Console.GetOpt
import System.Environment
import System.IO

import Text.Parsec.Error
import Parse


data Flags
  = Help
  | Stamp String

optDescrs = [Option ['h'] ["help"] (NoArg Help) "Print this message",
             Option ['s'] ["stamp"] (ReqArg Stamp "TITLE") "Generate a file, stamped with the date/time"]

helpMsg = usageInfo "" optDescrs

date = toGregorian <$> utctDay <$> getCurrentTime

natStrToLen n s = 
  let diff = n - length s in
  if diff > 0 then 
    replicate diff '0' ++ s
  else 
    drop diff s

showY = natStrToLen 4 . show
showM = natStrToLen 2 . show
showD = natStrToLen 2 . show

formatDate :: (Integer, Int, Int) -> String
formatDate (y,m,d) = intercalate "-" [
  showY y,
  showM m,
  showD d]

run [] = do
  inp <- getContents
  case parseCoqToMd inp of
    Left  err -> print err
    Right str -> putStrLn str

run [Stamp a] = do
  inp <- getContents
  case parseCoqToMd inp of
    Left  err -> print err
    Right str -> do
      ymd <- date
      let filename = formatDate ymd ++ "-" ++ a ++ ".md"
      writeFile filename $ unlines [
        header a ymd,
        str]
  where 
    header a date = unlines [
      "---",
      "layout: post",
      "title: " ++ a,
      "date: " ++ formatDate date, 
      "categories: Coq",
      "---"]

run [Help] = putStrLn helpMsg
run _      = putStrLn helpMsg
 
main :: IO ()
main = do 
  args <- getArgs
  let (flags, notFlags, errs) = getOpt RequireOrder optDescrs args
  if not $ null errs then 
    putStrLn $ unlines $ "Error: " : errs
  else 
    run flags
