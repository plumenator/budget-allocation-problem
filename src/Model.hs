module Model (
  add,
  Bill (..),
  billsFromInput,
  BillName (..),
  Category (..),
  DistrictName (..),
  Amount (..),
  District (..),
  districtsFromInput
  ) where

import Data.Map.Strict
import Data.Text

import InputSchema (Bill, BillName, Category, Amount (..), DistrictName)
import qualified InputSchema as I

billsFromInput :: I.Input -> Either String [Bill]
billsFromInput I.Input { I.bills = bills }
  | Prelude.null bills = Left "Expected at least one Bill"
  | otherwise = Right bills

data District = District {
  districtName :: DistrictName,
  availableFunds :: Amount,
  categoryDefaultFunding :: Map Category Amount,
  billSpecificFunding :: Map BillName Amount,
  caps :: Map Category Amount
  }
  deriving (Show, Eq)

toDistrict :: I.District -> Either String District
toDistrict I.District { I.districtName = dname,
                        I.availableFunds = afunds,
                        I.categoryDefaultFunding = defaults,
                        I.billSpecificFunding = specifics,
                        I.caps = caps } = Right District { districtName = dname,
                                                           availableFunds = afunds,
                                                           categoryDefaultFunding = toCategoryDefaultMap defaults,
                                                           billSpecificFunding = toBillSpecificFundingMap specifics,
                                                           caps = toCapsMap caps}

toCategoryDefaultMap :: [I.CategoryDefault] -> Map Category Amount
toCategoryDefaultMap = fromList . fmap toTuple where
  toTuple I.CategoryDefault { I.defaultCategory = c, I.defaultAmount = a } = (c, a)

toBillSpecificFundingMap :: [I.BillSpecific] -> Map BillName Amount
toBillSpecificFundingMap = fromList . fmap toTuple where
  toTuple I.BillSpecific { I.bill = b, I.specificAmount = a } = (b, a)

toCapsMap :: [I.Cap] -> Map Category Amount
toCapsMap = fromList . fmap toTuple where
  toTuple I.Cap { I.capCategory = b, I.capAmount = a } = (b, a)

districtsFromInput :: I.Input -> Either String [District]
districtsFromInput I.Input { I.districts = districts }
  | Prelude.null districts = Left "Expected at least one District"
  | otherwise = sequence $ fmap toDistrict districts

add :: Amount -> Amount -> Amount
add (Amount x) (Amount y) = Amount (x + y)
