{-# LANGUAGE OverloadedStrings #-}

module ModelSpec where

import Data.Map.Strict (empty)
import Test.Hspec

import InputSchema
import qualified Model as M

spec :: Spec
spec = do
  atLeastOneBill
  someDistricts

atLeastOneBill :: Spec
atLeastOneBill = do
  describe "Get at least one Bill from Input" $ do
    it "returns Right [Bill] when given an Input with at least one Bill" $
      M.billsFromInput Input { bills = [Bill {
                                        billName = BillName "An Act to Construct the Great Wall of Malodivo",
                                        category = Category "Defense",
                                        amount = Amount 200000
                                        }],
                              districts = [] } `shouldBe` Right [M.Bill {
                                        M.billName = M.BillName "An Act to Construct the Great Wall of Malodivo",
                                        M.category = M.Category "Defense",
                                        M.amount = Amount 200000
                                        }]
    it "returns Left String when given an Input with no Bills" $
      M.billsFromInput Input { bills = [],
                              districts = [] } `shouldBe` Left "Expected at least one Bill"

someDistricts :: Spec
someDistricts = do
  describe "Get some Districts from Input" $ do
    it "returns Right [District] when given an Input with Districts" $
      M.districtsFromInput Input { bills = [],
                                   districts = [District {
                                                   districtName = DistrictName "Tulsa",
                                                   availableFunds = Amount 10000,
                                                   categoryDefaultFunding = [],
                                                   billSpecificFunding =  [],
                                                   caps = []
                                                   }] } `shouldBe` Right [M.District {
                                                                             M.districtName = DistrictName "Tulsa",
                                                                             M.availableFunds = Amount 10000,
                                                                             M.categoryDefaultFunding = empty,
                                                                             M.billSpecificFunding =  empty,
                                                                             M.caps = empty}]
    it "returns Left String when given an Input with no Districts" $
      M.districtsFromInput Input { bills = [],
                                   districts = [] } `shouldBe` Left "Expected at least one District"
