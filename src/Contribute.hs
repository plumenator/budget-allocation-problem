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
  contribution bill = O.Contribution { O.billName = billName bill,
                                       O.funds = fmap (fund districtProvided totalProvidedFunds requiredFunds) allDistricts } where
    totalProvidedFunds = Prelude.foldr add (Amount 0) (fmap districtProvided allDistricts)
    districtProvided = billProvided (totalAllocated allBills) (totalCategoryAllocated (category bill) allBills) bill
    requiredFunds = amount bill

fund :: (District -> Amount) -> Amount -> Amount -> District -> O.Fund
fund districtProvided totalProvidedFunds requiredFunds district =
  O.Fund { O.district = districtName district,
           O.amount = min districtProvidedFunds (share contributionProportion requiredFunds) } where
  contributionProportion = ratio districtProvidedFunds totalProvidedFunds
  districtProvidedFunds = districtProvided district

billProvided :: (District -> Amount) -> (District -> Amount) -> Bill -> District -> Amount
billProvided totalAllocated totalCategoryAllocated bill district =
  case billCap bill (ratio billAllocated (totalCategoryAllocated district)) district of
    Just capped -> min uncapped capped
    Nothing -> uncapped
  where
    uncapped = min billAllocated (billAvailableFunds (ratio billAllocated (totalAllocated district)) district)
    billAllocated = billAllocation bill district

totalAllocated :: [Bill] -> District -> Amount
totalAllocated bills district = Prelude.foldr add (Amount 0) [billAllocation b district | b <- bills]

totalCategoryAllocated :: Category -> [Bill] -> District -> Amount
totalCategoryAllocated category bills district = Prelude.foldr add (Amount 0) [billAllocation b district | b <- bills, (Model.category b) == category]

billCap :: Bill -> Rational -> District -> Maybe Amount
billCap bill ratio district =  Map.lookup (category bill) (caps district) >>= return . share ratio

billAvailableFunds :: Rational -> District -> Amount
billAvailableFunds ratio district = share ratio (availableFunds district)

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

