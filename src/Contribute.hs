module Contribute (
  O.Contribution (..),
  contribute,
  O.Fund (..)
  ) where

import Data.Ratio
import qualified Data.Map.Strict as Map

import Model
import qualified OutputSchema as O

contribute :: [Bill] -> [District] -> [O.Contribution]
contribute allBills allDistricts = fmap contribution allBills where
  contribution bill = O.Contribution { O.billName = billName bill, O.funds = fmap (fund bill) allDistricts }
  fund bill district = O.Fund { O.district = districtName district, O.amount = min (provided district) (share (contributionProportion district) (amount bill)) } where
    contributionProportion district = ratio (provided district) totalProvidedFunds
    totalProvidedFunds = Prelude.foldr add (Amount 0) (fmap provided allDistricts)
    provided district = min billAllocated (billAvailableFunds bill (ratio billAllocated totalAllocated) district) where
      billAllocated = billAllocation bill district
      totalAllocated = Prelude.foldr add (Amount 0) [billAllocation b district | b <- allBills]

billAvailableFunds :: Bill -> Rational -> District -> Amount
billAvailableFunds bill ratio district = share ratio (availableFunds district)

share :: Rational -> Amount -> Amount
share ratio (Amount total) = Amount $ (numerator ratio) * total `div` (denominator ratio)

billAllocation :: Bill -> District -> Amount
billAllocation bill district = Map.findWithDefault (categoryAllocation (category bill) district) (billName bill) (billSpecificFunding district)

categoryAllocation :: Category -> District -> Amount
categoryAllocation  category District { categoryDefaultFunding = defaults } = Map.findWithDefault (Amount 0) category defaults

add :: Amount -> Amount -> Amount
add (Amount x) (Amount y) = Amount (x + y)

ratio :: Amount -> Amount -> Rational
ratio _ (Amount 0) = 0
ratio (Amount n) (Amount d) = fromIntegral n / fromIntegral d

