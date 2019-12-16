{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  testProcessInputBytes

testProcessInputBytes :: Spec
testProcessInputBytes = do
  describe "Process JSON input" $ do
    it "returns output JSON when given a JSON input" $
      processInputBytes inputJSON `shouldBe` Right outputJSON

inputJSON ="{\
\  \"bills\": [\
\    {\
\      \"name\": \"An Act to Construct the Great Wall of Malodivo\",\
\      \"category\": \"Defense\",\
\      \"amount\": 200000\
\    },\
\    {\
\      \"name\": \"An Act to Construct Shelters for the Homeless\",\
\      \"category\": \"Welfare\",\
\      \"amount\": 40000\
\    },\
\    {\
\      \"name\": \"An Act to Fund the Development of Longer-Lasting Paper\",\
\      \"category\": \"Science\",\
\      \"amount\": 14000\
\    },\
\    {\
\      \"name\": \"An Act to Increase Retirement Benefits for Veterans\",\
\      \"category\": \"Welfare\",\
\      \"amount\": 90000\
\    }\
\  ],\
\  \"districts\": [\
\    {\
\      \"name\": \"Palolene\",\
\      \"availableFunds\": 200000,\
\      \"categoryDefaultFunding\": [\
\        {\
\          \"category\": \"Defense\",\
\          \"amount\": 1000\
\        },\
\        {\
\          \"category\": \"Welfare\",\
\          \"amount\": 3000\
\        },\
\        {\
\          \"category\": \"Science\",\
\          \"amount\": 5000\
\        }\
\      ],\
\      \"billSpecificFunding\": [\
\        {\
\          \"bill\": \"An Act to Increase Retirement Benefits for Veterans\",\
\          \"amount\": 500\
\        },\
\        {\
\          \"bill\": \"An Act to Fund the Development of Longer-Lasting Paper\",\
\          \"amount\": 7500\
\        }\
\      ],\
\      \"caps\": [\
\        {\
\          \"category\": \"Defense\",\
\          \"amount\": 3000\
\        },\
\        {\
\          \"category\": \"Welfare\",\
\          \"amount\": 3199\
\        },\
\        {\
\          \"category\": \"Science\",\
\          \"amount\": 15000\
\        }\
\      ]\
\    },\
\    {\
\      \"name\": \"Southern Palolene\",\
\      \"availableFunds\": 150000,\
\      \"categoryDefaultFunding\": [\
\        {\
\          \"category\": \"Defense\",\
\          \"amount\": 500\
\        },\
\        {\
\          \"category\": \"Welfare\",\
\          \"amount\": 5000\
\        },\
\        {\
\          \"category\": \"Science\",\
\          \"amount\": 2000\
\        }\
\      ],\
\      \"billSpecificFunding\": [],\
\      \"caps\": [\
\        {\
\          \"category\": \"Defense\",\
\          \"amount\": 10000\
\        },\
\        {\
\          \"category\": \"Welfare\",\
\          \"amount\": 10000\
\        },\
\        {\
\          \"category\": \"Science\",\
\          \"amount\": 10000\
\        }\
\      ]\
\    },\
\    {\
\      \"name\": \"Lakos\",\
\      \"availableFunds\": 400000,\
\      \"categoryDefaultFunding\": [\
\        {\
\          \"category\": \"Defense\",\
\          \"amount\": 10000\
\        },\
\        {\
\          \"category\": \"Welfare\",\
\          \"amount\": 1000\
\        },\
\        {\
\          \"category\": \"Science\",\
\          \"amount\": 500\
\        }\
\      ],\
\      \"billSpecificFunding\": [\
\        {\
\          \"bill\": \"An Act to Construct the Great Wall of Malodivo\",\
\          \"amount\": 100000\
\        },\
\        {\
\          \"bill\": \"An Act to Fund the Development of Longer-Lasting Paper\",\
\          \"amount\": 0\
\        }\
\      ],\
\      \"caps\": [\
\        {\
\          \"category\": \"Defense\",\
\          \"amount\": 30000\
\        },\
\        {\
\          \"category\": \"Welfare\",\
\          \"amount\": 3000\
\        },\
\        {\
\          \"category\": \"Science\",\
\          \"amount\": 1000\
\        }\
\      ]\
\    }\
\  ]\
\}"

outputJSON = "{\"balances\":[{\"amount\":188301,\"district\":\"Palolene\"},{\"amount\":137500,\"district\":\"Southern Palolene\"},{\"amount\":368000,\"district\":\"Lakos\"}],\"deficits\":[{\"amount\":168500,\"billName\":\"An Act to Construct the Great Wall of Malodivo\"},{\"amount\":31258,\"billName\":\"An Act to Construct Shelters for the Homeless\"},{\"amount\":4500,\"billName\":\"An Act to Fund the Development of Longer-Lasting Paper\"},{\"amount\":83543,\"billName\":\"An Act to Increase Retirement Benefits for Veterans\"}],\"contributions\":[{\"billName\":\"An Act to Construct the Great Wall of Malodivo\",\"funds\":[{\"amount\":1000,\"district\":\"Palolene\"},{\"amount\":500,\"district\":\"Southern Palolene\"},{\"amount\":30000,\"district\":\"Lakos\"}]},{\"billName\":\"An Act to Construct Shelters for the Homeless\",\"funds\":[{\"amount\":2742,\"district\":\"Palolene\"},{\"amount\":5000,\"district\":\"Southern Palolene\"},{\"amount\":1000,\"district\":\"Lakos\"}]},{\"billName\":\"An Act to Fund the Development of Longer-Lasting Paper\",\"funds\":[{\"amount\":7500,\"district\":\"Palolene\"},{\"amount\":2000,\"district\":\"Southern Palolene\"},{\"amount\":0,\"district\":\"Lakos\"}]},{\"billName\":\"An Act to Increase Retirement Benefits for Veterans\",\"funds\":[{\"amount\":457,\"district\":\"Palolene\"},{\"amount\":5000,\"district\":\"Southern Palolene\"},{\"amount\":1000,\"district\":\"Lakos\"}]}]}"
