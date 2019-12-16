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
  let contributions = contribute bills districts
  let deficits = fmap deficit (Prelude.zip bills contributions)
  return $ encode Output { contributions = contributions, deficits = deficits }

deficit :: (Bill, Contribution) -> Deficit
deficit (bill, contribution) =
  Deficit { deficitBillName = Model.billName bill,
            deficitAmount = (Model.amount bill) `sub` (Prelude.foldr add (Amount 0) amounts) }
  where
    amounts = fmap OutputSchema.amount (funds contribution)
