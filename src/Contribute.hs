module Contribute (
  O.Contribution (..),
  contribute,
  billAllocationProportion,
  billAvailableFunds,
  O.Fund (..)
  ) where

import Data.Ratio
import qualified Data.Map.Strict as Map

import Model
import qualified OutputSchema as O

contribute :: Bill -> [District] -> O.Contribution
contribute Bill { Model.billName = billName, category = category, Model.amount = requiredFunds } allDistricts = O.Contribution { O.billName = billName, O.funds = fmap contribution allDistricts } where
  contribution district = O.Fund { O.district = districtName district, O.amount = min (billAvailableFunds category district) (share (contributionProportion district) requiredFunds) }
  contributionProportion district = ratio (billAvailableFunds category district) totalAvailableFunds
  totalAvailableFunds = Prelude.foldr add (Amount 0) (fmap (billAvailableFunds category) allDistricts)

billAllocationProportion :: Category -> District -> Rational
billAllocationProportion category district@District { categoryDefaultFunding = defaults }
  = ratio (categoryAllocation category district) totalAllocation where
  totalAllocation = Map.foldl' add (Amount 0) defaults

categoryAllocation :: Category -> District -> Amount
categoryAllocation  category District { categoryDefaultFunding = defaults } = Map.findWithDefault (Amount 0) category defaults

add :: Amount -> Amount -> Amount
add (Amount x) (Amount y) = Amount (x + y)

ratio :: Amount -> Amount -> Rational
ratio _ (Amount 0) = 0
ratio (Amount n) (Amount d) = fromIntegral n / fromIntegral d

billAvailableFunds :: Category -> District -> Amount
billAvailableFunds category district@District { availableFunds = totalAvailable } =
  min allocated available where
  allocated = categoryAllocation category district
  available = share (billAllocationProportion category district) totalAvailable

share :: Rational -> Amount -> Amount
share ratio (Amount total) = Amount $ (numerator ratio) * total `div` (denominator ratio)
