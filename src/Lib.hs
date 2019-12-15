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
  bills <- billsFromInput input
  districts <- districtsFromInput input
  return $ encode Output { contributions = contribute bills districts }
