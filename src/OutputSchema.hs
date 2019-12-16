{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module OutputSchema (
  Contribution (..),
  Deficit (..),
  Balance (..),
  Amount (..),
  BillName (..),
  DistrictName (..),
  Fund (..),
  Output (..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics

import Model (Amount (..), BillName (..), DistrictName (..))

data Output = Output {
  contributions :: [Contribution],
  deficits :: [Deficit],
  balances :: [Balance]
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Contribution = Contribution {
  billName :: BillName,
  funds :: [Fund]
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Fund = Fund {
  district :: DistrictName,
  amount :: Amount
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Deficit = Deficit {
  deficitBillName :: BillName,
  deficitAmount :: Amount
  }
  deriving (Show, Generic, Eq)

modifyDeficitField "deficitBillName" = "billName"
modifyDeficitField "deficitAmount" = "amount"
modifyDeficitField fieldName  = fieldName

instance ToJSON Deficit where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyDeficitField
    }

instance FromJSON Deficit where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyDeficitField
    }

data Balance = Balance {
  balanceDistrict :: DistrictName,
  balanceAmount :: Amount
  }
  deriving (Show, Generic, Eq)

modifyBalanceField "balanceDistrict" = "district"
modifyBalanceField "balanceAmount" = "amount"
modifyBalanceField fieldName  = fieldName

instance ToJSON Balance where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = modifyBalanceField
    }

instance FromJSON Balance where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = modifyBalanceField
    }
