{-# LANGUAGE OverloadedStrings #-}

module ModelSpec where

import Data.Map.Strict (empty)
import Test.Hspec

import InputSchema
import qualified Model as M

spec :: Spec
spec = do
  exactlyOneBill
  someDistricts

exactlyOneBill :: Spec
exactlyOneBill = do
  describe "Get exactly one Bill from Input" $ do
    it "returns Right Bill when given an Input with one Bill" $
      M.billFromInput Input { bills = [Bill {
                                        billName = BillName "An Act to Construct the Great Wall of Malodivo",
                                        category = Defense,
                                        amount = Amount 200000
                                        }],
                              districts = [] } `shouldBe` Right M.Bill {
                                        M.billName = M.BillName "An Act to Construct the Great Wall of Malodivo",
                                        M.category = M.Defense,
                                        M.amount = Amount 200000
                                        }
    it "returns Left String when given an Input with multiple Bills" $
      M.billFromInput Input { bills = [Bill {
                                        billName = BillName "An Act to Construct the Great Wall of Malodivo",
                                        category = Defense,
                                        amount = Amount 200000
                                        },
                                      Bill {
                                        billName = BillName "An Act to Construct Shelters for the Homeless",
                                        category = Welfare,
                                        amount = Amount 40000
                                        }],
                              districts = [] } `shouldBe` Left "Expected exactly one Bill"
    it "returns Left String when given an Input with no Bills" $
      M.billFromInput Input { bills = [],
                              districts = [] } `shouldBe` Left "Expected exactly one Bill"

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
                                                                             M.categoryDefaultFunding = empty}]
    it "returns Left String when given an Input with no Districts" $
      M.districtsFromInput Input { bills = [],
                                   districts = [] } `shouldBe` Left "Expected at least one District"
