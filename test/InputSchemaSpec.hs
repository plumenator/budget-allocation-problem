{-# LANGUAGE OverloadedStrings #-}

module InputSchemaSpec where

import Data.Aeson
import Test.Hspec

import InputSchema

spec :: Spec
spec = do
  decodeBillName
  decodeCategory
  decodeAmount
  decodeBill
  decodeDistrictName
  decodeCategoryDefault
  decodeBillSpecific
  decodeCap
  decodeInput

decodeBillName :: Spec
decodeBillName = do
  describe "Decode BillName from JSON" $ do
    it "returns Just BillName when given a JSON string" $
      decode "\"The name of a bill\"" `shouldBe` Just (BillName "The name of a bill")

decodeCategory :: Spec
decodeCategory = do
  describe "Decode Category from JSON" $ do
    it "returns Just (Category \"Defense\") when given \"Defense\"" $
      decode "\"Defense\"" `shouldBe` Just (Category "Defense")
    it "returns Just (Category \"Welfare\") when given \"Welfare\"" $
      decode "\"Welfare\"" `shouldBe` Just (Category "Welfare")
    it "returns Just (Category \"Science\") when given \"Science\"" $
      decode "\"Science\"" `shouldBe` Just (Category "Science")

decodeAmount :: Spec
decodeAmount = do
  describe "Decode Amount from JSON" $ do
    it "returns Just Amount when given a JSON number" $
      decode "1234" `shouldBe` Just (Amount 1234)

decodeBill :: Spec
decodeBill = do
  describe "Decode Bill from JSON" $ do
    it "returns Right Bill when given a JSON Bill" $
      eitherDecode "{ \
                   \ \"name\": \"An Act to Construct the Great Wall of Malodivo\", \
                   \ \"category\": \"Defense\", \
                   \ \"amount\": 200000 \
                   \ }" `shouldBe` Right (Bill {
                                             billName = BillName "An Act to Construct the Great Wall of Malodivo",
                                             category = Category "Defense",
                                             amount = Amount 200000
                                             })
    it "returns Nothing when given a bad JSON Bill" $
      (decode "{ \
             \ \"nameOfBill\": \"An Act to Construct the Great Wall of Malodivo\", \
             \ \"category\": \"Defense\", \
             \ \"amount\": 200000 \
             \ }" :: Maybe Bill) `shouldBe` Nothing

decodeDistrictName :: Spec
decodeDistrictName = do
  describe "Decode DistrictName from JSON" $ do
    it "returns Just DistrictName when given a JSON string" $
      decode "\"The name of a district\"" `shouldBe` Just (DistrictName "The name of a district")

decodeCategoryDefault :: Spec
decodeCategoryDefault = do
  describe "Decode CategoryDefault from JSON" $ do
    it "returns Right CategoryDefault when given a JSON CategoryDefault" $
      eitherDecode "{ \
                   \ \"category\": \"Defense\", \
                   \ \"amount\": 1000 \
                   \ }" `shouldBe` Right (CategoryDefault {
                                             defaultCategory = Category "Defense",
                                             defaultAmount = Amount 1000
                                             })

decodeBillSpecific :: Spec
decodeBillSpecific = do
  describe "Decode BillSpecific from JSON" $ do
    it "returns Just BillSpecific when given a JSON BillSpecific" $
      eitherDecode "{ \
                   \ \"bill\": \"The name of a bill\", \
                   \ \"amount\": 1000 \
                   \ }" `shouldBe` Right (BillSpecific {
                                             bill = BillName "The name of a bill",
                                             specificAmount = Amount 1000
                                             })

decodeCap :: Spec
decodeCap = do
  describe "Decode Cap from JSON" $ do
    it "returns Right Cap when given a JSON Cap" $
      eitherDecode "{ \
                   \ \"category\": \"Defense\", \
                   \ \"amount\": 1000 \
                   \ }" `shouldBe` Right (Cap {
                                             capCategory = Category "Defense",
                                             capAmount = Amount 1000
                                             })

sampleInputJSON = "{ \
              \  \"bills\": [ \
              \    { \
              \      \"name\": \"An Act to Construct the Great Wall of Malodivo\", \
              \      \"category\": \"Defense\", \
              \      \"amount\": 200000 \
              \    }, \
              \    { \
              \      \"name\": \"An Act to Construct Shelters for the Homeless\", \
              \      \"category\": \"Welfare\", \
              \      \"amount\": 40000 \
              \    } \
              \  ], \
              \  \"districts\": [ \
              \    { \
              \      \"name\": \"Palolene\", \
              \      \"availableFunds\": 200000, \
              \      \"categoryDefaultFunding\": [ \
              \        { \
              \          \"category\": \"Defense\", \
              \          \"amount\": 1000 \
              \        }, \
              \        { \
              \          \"category\": \"Welfare\", \
              \          \"amount\": 3000 \
              \        }, \
              \        { \
              \          \"category\": \"Science\", \
              \          \"amount\": 5000 \
              \        } \
              \      ], \
              \      \"billSpecificFunding\": [ \
              \        { \
              \          \"bill\": \"An Act to Increase Retirement Benefits for Veterans\", \
              \          \"amount\": 500 \
              \        }, \
              \        { \
              \          \"bill\": \"An Act to Fund the Development of Longer-Lasting Paper\", \
              \          \"amount\": 7500 \
              \        } \
              \      ], \
              \      \"caps\": [ \
              \        { \
              \          \"category\": \"Defense\", \
              \          \"amount\": 3000 \
              \        }, \
              \        { \
              \          \"category\": \"Welfare\", \
              \          \"amount\": 3199 \
              \        }, \
              \        { \
              \          \"category\": \"Science\", \
              \          \"amount\": 15000 \
              \        } \
              \      ] \
              \    }, \
              \    { \
              \      \"name\": \"Southern Palolene\", \
              \      \"availableFunds\": 150000, \
              \      \"categoryDefaultFunding\": [ \
              \        { \
              \          \"category\": \"Defense\", \
              \          \"amount\": 500 \
              \        }, \
              \        { \
              \          \"category\": \"Welfare\", \
              \          \"amount\": 5000 \
              \        }, \
              \        { \
              \          \"category\": \"Science\", \
              \          \"amount\": 2000 \
              \        } \
              \      ], \
              \      \"billSpecificFunding\": [], \
              \      \"caps\": [ \
              \        { \
              \          \"category\": \"Defense\", \
              \          \"amount\": 10000 \
              \        }, \
              \        { \
              \          \"category\": \"Welfare\", \
              \          \"amount\": 10000 \
              \        }, \
              \        { \
              \          \"category\": \"Science\", \
              \          \"amount\": 10000 \
              \        } \
              \      ] \
              \    } \
              \  ] \
              \ }"

sampleInput = Input {
  bills = [
    Bill {
      billName = BillName "An Act to Construct the Great Wall of Malodivo",
      category = Category "Defense",
      amount = Amount 200000
    },
    Bill {
      billName = BillName "An Act to Construct Shelters for the Homeless",
      category = Category "Welfare",
      amount = Amount 40000
    }
  ],
  districts = [
    District {
      districtName = DistrictName "Palolene",
      availableFunds = Amount 200000,
      categoryDefaultFunding = [
        CategoryDefault {
          defaultCategory = Category "Defense",
          defaultAmount = Amount 1000
        },
        CategoryDefault {
          defaultCategory = Category "Welfare",
          defaultAmount = Amount 3000
        },
        CategoryDefault {
          defaultCategory = Category "Science",
          defaultAmount = Amount 5000
        }
      ],
      billSpecificFunding = [
        BillSpecific {
          bill = BillName "An Act to Increase Retirement Benefits for Veterans",
          specificAmount = Amount 500
        },
        BillSpecific {
          bill = BillName "An Act to Fund the Development of Longer-Lasting Paper",
          specificAmount = Amount 7500
        }
      ],
      caps = [
        Cap {
          capCategory = Category "Defense",
          capAmount = Amount 3000
        },
        Cap {
          capCategory = Category "Welfare",
          capAmount = Amount 3199
        },
        Cap {
          capCategory = Category "Science",
          capAmount = Amount 15000
        }
      ]
    },
    District {
      districtName = DistrictName "Southern Palolene",
      availableFunds = Amount 150000,
      categoryDefaultFunding = [
        CategoryDefault {
          defaultCategory = Category "Defense",
          defaultAmount = Amount 500
        },
        CategoryDefault {
          defaultCategory = Category "Welfare",
          defaultAmount = Amount 5000
        },
        CategoryDefault {
          defaultCategory = Category "Science",
          defaultAmount = Amount 2000
        }
      ],
      billSpecificFunding = [],
      caps = [
        Cap {
          capCategory = Category "Defense",
          capAmount = Amount 10000
        },
        Cap {
          capCategory = Category "Welfare",
          capAmount = Amount 10000
        },
        Cap {
          capCategory = Category "Science",
          capAmount = Amount 10000
        }
      ]
    }
  ]
}

decodeInput :: Spec
decodeInput = do
  describe "Decode Input from JSON" $ do
    it "returns Right Input when given a JSON Cap" $
      eitherDecode sampleInputJSON `shouldBe` Right sampleInput
