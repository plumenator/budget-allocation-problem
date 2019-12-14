{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module OutputSchema where

import Data.Aeson
import Data.Text
import GHC.Generics

data Output = Output {
  bills :: [Bill]
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Bill = Bill {
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
