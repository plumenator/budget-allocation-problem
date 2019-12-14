{-# LANGUAGE OverloadedStrings #-}

module OutputSchemaSpec where

import Data.Aeson
import Test.Hspec

import OutputSchema

spec :: Spec
spec = do
  encodeBillName
  encodeDistrictName
  encodeAmount
  encodeFund
  encodeBill
  encodeOutput

encodeBillName :: Spec
encodeBillName = do
  describe "Encode BillName to JSON" $ do
    it "returns a JSON string when given a BillName" $
      encode (BillName "The name of a bill") `shouldBe` "\"The name of a bill\""

encodeDistrictName :: Spec
encodeDistrictName = do
  describe "Encode DistrictName to JSON" $ do
    it "returns a JSON String when given a DistrictName" $
      encode (DistrictName "The name of a district") `shouldBe` "\"The name of a district\""

encodeAmount :: Spec
encodeAmount = do
  describe "Encode Amount to JSON" $ do
    it "returns a JSON number when given an Amount" $
      encode (Amount 1234) `shouldBe` "1234"

encodeFund :: Spec
encodeFund = do
  describe "Encode Fund to JSON" $ do
    it "returns JSON when given a Fund" $
      encode Fund { district = DistrictName "Tulsa", amount = Amount 1000 } `shouldBe` "{\
             \\"amount\":1000,\
             \\"district\":\"Tulsa\"\
             \}"

encodeBill :: Spec
encodeBill = do
  describe "Encode Bill to JSON" $ do
    it "returns JSON when given a Bill" $
      encode Bill {
      name = BillName "A Bill",
      funds = [Fund {
                  district = DistrictName "Tulsa",
                  amount = Amount 1000
                  },
                Fund {
                  district = DistrictName "Idaho",
                  amount = Amount 3333
                  }]
      } `shouldBe` "{\
             \\"name\":\"A Bill\",\
             \\"funds\":[\
               \{\
                 \\"amount\":1000,\
                 \\"district\":\"Tulsa\"\
               \},\
               \{\
                 \\"amount\":3333,\
                 \\"district\":\"Idaho\"\
               \}\
             \]\
           \}"

encodeOutput :: Spec
encodeOutput = do
  describe "Encode Output to JSON" $ do
    it "returns JSON when given an Output" $
      encode Output {
      bills = [Bill {
                  name = BillName "A Bill",
                  funds = [Fund {
                              district = DistrictName "Tulsa",
                              amount = Amount 1000
                              },
                           Fund {
                              district = DistrictName "Idaho",
                              amount = Amount 3333
                              }]
                  },
                Bill {
                  name = BillName "Another Bill",
                  funds = [Fund {
                              district = DistrictName "Tulsa",
                              amount = Amount 4444
                              },
                           Fund {
                              district = DistrictName "Idaho",
                              amount = Amount 5555
                              }]
                  }]
      } `shouldBe` "{\"bills\":[{\"name\":\"A Bill\",\"funds\":[{\"amount\":1000,\"district\":\"Tulsa\"},{\"amount\":3333,\"district\":\"Idaho\"}]},{\"name\":\"Another Bill\",\"funds\":[{\"amount\":4444,\"district\":\"Tulsa\"},{\"amount\":5555,\"district\":\"Idaho\"}]}]}"
