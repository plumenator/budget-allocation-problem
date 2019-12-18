module Lib (
  processInputBytes
  ) where

import Data.Aeson
import Data.ByteString.Lazy

import Contribute
import InputSchema (Input)
import Model
import OutputSchema

processInputBytes :: ByteString -> Either String ByteString
processInputBytes inputBytes = do
  input <- eitherDecode inputBytes
  output <- processInput input
  return $ encode output

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
