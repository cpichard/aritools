module Main where

import Codec.Picture.Ari.Header
import Applications.AriHeader.ExportCSV
import System.Environment (getArgs)

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: ariheader [FILENAMES...]"
    a -> extractHeaders a 
    
extractHeaders :: [String] -> IO ()
extractHeaders (c:cs) = do
  header <- readHeader c
  let csvFileName = ((reverse ( drop 3 (reverse c))) ++ "csv")
  writeFile csvFileName (exportCSV header) 
  putStrLn $ "wrote " ++ csvFileName
  extractHeaders cs
extractHeaders [] = return () 

