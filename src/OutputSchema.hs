{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module OutputSchema (
  Contribution (..),
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
  contributions :: [Contribution]
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
