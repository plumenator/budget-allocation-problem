module Lib (
  processInputBytes
  ) where

import Data.Aeson
import Data.ByteString.Lazy

import Contribute
import Model
import OutputSchema

processInputBytes :: ByteString -> Either String ByteString
processInputBytes inputBytes = do
  input <- eitherDecode inputBytes
  bill <- billFromInput input
  districts <- districtsFromInput input
  return $ encode Output { contributions = [contribute bill districts] }
