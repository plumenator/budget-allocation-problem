module Main where

import Data.ByteString.Lazy.Char8 as B

import Lib

main :: IO ()
main = do
  inputBytes <- B.getContents
  case processInputBytesPretty inputBytes of
    Right outputBytes -> B.putStrLn outputBytes
    Left errorString -> Prelude.putStrLn errorString
