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
contribute allBills allDistricts = fmap (contribution (totalAllocated allBills) (totalCategoryAllocated allBills) allDistricts) allBills

contribution :: (District -> Amount) -> (Category -> District -> Amount) -> [District] -> Bill -> O.Contribution
contribution totalAllocated totalCategoryAllocated allDistricts bill =
  O.Contribution { O.billName = billName bill,
                   O.funds = fmap (fund districtProvided totalProvidedFunds requiredFunds) allDistricts } where
  totalProvidedFunds = Prelude.foldr add (Amount 0) (fmap districtProvided allDistricts)
  districtProvided = billProvided totalAllocated totalCategoryAllocated bill
  requiredFunds = amount bill

fund :: (District -> Amount) -> Amount -> Amount -> District -> O.Fund
fund districtProvided totalProvidedFunds requiredFunds district =
  O.Fund { O.district = districtName district,
           -- proportionality of contribution
           O.amount = min districtProvidedFunds (share contributionProportion requiredFunds) } where
  contributionProportion = ratio districtProvidedFunds totalProvidedFunds
  districtProvidedFunds = districtProvided district

billProvided :: (District -> Amount) -> (Category -> District -> Amount) -> Bill -> District -> Amount
billProvided totalAllocated totalCategoryAllocated bill district =
  -- proportionality of bill allocation (category default or bill specific or share of the cap)
  min billCappedAndAllocated (billAvailableFunds (ratio billCappedAndAllocated (totalAllocated district)) district) where
  billCappedAndAllocated = billCappedAllocation totalCategoryAllocated bill district

totalAllocated :: [Bill] -> District -> Amount
totalAllocated allBills district = Prelude.foldr add (Amount 0) [billCappedAllocation (totalCategoryAllocated allBills) b district | b <- allBills]

billCappedAllocation :: (Category -> District -> Amount) -> Bill -> District -> Amount
billCappedAllocation totalCategoryAllocated bill district = maybe uncapped (min uncapped) capped where
  uncapped = billAllocation bill district
  -- proportionality of category cap
  capped = billCap (category bill) (ratio uncapped (totalCategoryAllocated (category bill) district)) district

totalCategoryAllocated :: [Bill] -> Category -> District -> Amount
totalCategoryAllocated bills category district = Prelude.foldr add (Amount 0) [billAllocation b district | b <- bills, (Model.category b) == category]

billCap :: Category -> Rational -> District -> Maybe Amount
billCap category ratio district =  Map.lookup category (caps district) >>= return . share ratio

billAvailableFunds :: Rational -> District -> Amount
billAvailableFunds ratio district = share ratio (availableFunds district)

share :: Rational -> Amount -> Amount
share ratio (Amount total) = Amount $ (numerator ratio) * total `div` (denominator ratio)

billAllocation :: Bill -> District -> Amount
billAllocation bill district = Map.findWithDefault (categoryAllocation (category bill) district) (billName bill) (billSpecificFunding district)

categoryAllocation :: Category -> District -> Amount
categoryAllocation  category District { categoryDefaultFunding = defaults } = Map.findWithDefault (Amount 0) category defaults

ratio :: Amount -> Amount -> Rational
ratio _ (Amount 0) = 0
ratio (Amount n) (Amount d) = fromIntegral n / fromIntegral d

