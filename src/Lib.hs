module Lib (
  processInputBytes,
  processInputBytesPretty,
  processInputText,
  processInputTextPretty,
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Bifunctor
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

import Contribute
import InputSchema (Input)
import Model
import OutputSchema

processInputText :: Text -> Either String Text
processInputText inputText = processInputBytes (encodeUtf8 inputText)
                             >>= first show . decodeUtf8'

processInputTextPretty :: Text -> Either String Text
processInputTextPretty inputText = processInputBytesPretty (encodeUtf8 inputText)
                                   >>= first show . decodeUtf8'

processInputBytes :: ByteString -> Either String ByteString
processInputBytes inputBytes = do
  input <- eitherDecode inputBytes
  output <- processInput input
  return $ encode output

processInputBytesPretty :: ByteString -> Either String ByteString
processInputBytesPretty inputBytes = do
  input <- eitherDecode inputBytes
  output <- processInput input
  return $ encodePretty output

processInput :: Input -> Either String Output
processInput input = do
  bills <- billsFromInput input
  districts <- districtsFromInput input
  let contributions = contribute bills districts
  let deficits = fmap deficit (Prelude.zip bills contributions)
  let balances = fmap (balance contributions) districts
  return $ Output { contributions = contributions, deficits = deficits, balances = balances }

deficit :: (Bill, Contribution) -> Deficit
deficit (bill, contribution) =
  Deficit { deficitBillName = Model.billName bill,
            deficitAmount = (Model.amount bill) `sub` (Prelude.foldr add (Amount 0) amounts) }
  where
    amounts = fmap OutputSchema.amount (funds contribution)

balance :: [Contribution] -> District -> Balance
balance contributions district =
  Balance { balanceDistrict = districtName district,
            balanceAmount = (availableFunds district) `sub` (Prelude.foldr add (Amount 0) amounts) }
  where
    amounts = [OutputSchema.amount fund | contribution <- contributions, fund <- funds contribution, (OutputSchema.district fund) == (districtName district)]
