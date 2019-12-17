{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module InputSchema where

import Data.Aeson
import Data.Text
import GHC.Generics

data Input = Input {
  bills :: [Bill],
  districts :: [District]
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Bill = Bill {
  billName :: BillName,
  category :: Category,
  amount :: Amount
  }
  deriving (Show, Generic, Eq)

modifyBillField "billName" = "name"
modifyBillField fieldName  = fieldName

instance ToJSON Bill where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyBillField
    }

instance FromJSON Bill where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyBillField
    }

newtype BillName = BillName Text
  deriving (Show, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Eq, Ord)

data Category = Category Text
  deriving (Show, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Eq, Ord)

-- TODO: Customize FromJSON to reject negative numbers
newtype Amount = Amount Integer
  deriving (Show, Generic, ToJSON, FromJSON, Eq, Ord)

data District = District {
  districtName :: DistrictName,
  availableFunds :: Amount,
  categoryDefaultFunding :: [CategoryDefault],
  billSpecificFunding :: [BillSpecific],
  caps :: [Cap]
  }
  deriving (Show, Generic, Eq)

modifyDistrictField "districtName" = "name"
modifyDistrictField fieldName  = fieldName

instance ToJSON District where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyDistrictField
    }

instance FromJSON District where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyDistrictField
    }

newtype DistrictName = DistrictName Text
  deriving (Show, Generic, ToJSON, FromJSON, Eq, Ord)

data CategoryDefault = CategoryDefault { defaultCategory :: Category, defaultAmount :: Amount }
  deriving (Show, Generic, Eq)

modifyCategoryDefaultField "defaultAmount" = "amount"
modifyCategoryDefaultField "defaultCategory" = "category"
modifyCategoryDefaultField fieldName  = fieldName

instance ToJSON CategoryDefault where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyCategoryDefaultField
    }

instance FromJSON CategoryDefault where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyCategoryDefaultField
    }

data BillSpecific = BillSpecific { bill :: BillName, specificAmount :: Amount }
  deriving (Show, Generic, Eq)

modifyBillSpecificField "specificAmount" = "amount"
modifyBillSpecificField fieldName  = fieldName

instance ToJSON BillSpecific where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyBillSpecificField
    }

instance FromJSON BillSpecific where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyBillSpecificField
    }

data Cap = Cap { capCategory :: Category, capAmount :: Amount }
  deriving (Show, Generic, Eq)

modifyCapField "capAmount" = "amount"
modifyCapField "capCategory" = "category"
modifyCapField fieldName  = fieldName

instance ToJSON Cap where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyCapField
    }

instance FromJSON Cap where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyCapField
    }
