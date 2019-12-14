module Model (
  Bill (..),
  billFromInput,
  BillName (..),
  Category (..),
  Amount,
  District (..),
  districtsFromInput
  ) where

import Data.Map.Strict
import Data.Text

import InputSchema (Bill, BillName, Category, Amount, DistrictName)
import qualified InputSchema as I

billFromInput :: I.Input -> Either String Bill
billFromInput  = takeOneOrFail . I.bills where
  takeOneOrFail [x] = Right x
  takeOneOrFail _ = Left "Expected exactly one Bill"

data District = District {
  districtName :: DistrictName,
  availableFunds :: Amount,
  categoryDefaultFunding :: Map Category Amount
  }
  deriving (Show, Eq)

toDistrict :: I.District -> District
toDistrict I.District { I.districtName = dname,
                        I.availableFunds = afunds,
                        I.categoryDefaultFunding = defaults } = District { districtName = dname,
                                                                           availableFunds = afunds,
                                                                           categoryDefaultFunding = toCategoryDefaultMap defaults }

toCategoryDefaultMap :: [I.CategoryDefault] -> Map Category Amount
toCategoryDefaultMap = fromList . fmap toTuple where
  toTuple I.CategoryDefault { I.defaultCategory = c, I.defaultAmount = a } = (c, a)

districtsFromInput :: I.Input -> Either String [District]
districtsFromInput I.Input { I.districts = districts }
  | Prelude.null districts = Left "Expected at least one District"
  | otherwise = Right (fmap toDistrict districts)
