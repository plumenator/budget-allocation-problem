module Main where

import qualified Data.Text.Lazy.IO as T

import Lib

main :: IO ()
main = do
  inputText <- T.getContents
  case processInputTextPretty inputText of
    Right outputText -> T.putStrLn outputText
    Left errorString -> Prelude.putStrLn errorString
