{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module OutputSchema where

import Data.Aeson
import Data.Text
import GHC.Generics

data Output = Output {
  contributions :: [Contribution]
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Contribution = Contribution {
  name :: BillName,
  funds :: [Fund]
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype BillName = BillName Text
  deriving (Show, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Eq, Ord)

data Fund = Fund {
  district :: DistrictName,
  amount :: Amount
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype DistrictName = DistrictName Text
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype Amount = Amount Integer
  deriving (Show, Generic, ToJSON, FromJSON, Eq)
