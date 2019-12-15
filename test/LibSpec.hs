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

inputJSON = "{\
\\"bills\":[\
\{\
\\"name\":\"An Act to Construct the Great Wall of Malodivo\",\
\\"category\":\"Defense\",\
\\"amount\":200000\
\}\
\],\
\\"districts\":[\
\{\
\\"name\":\"Palolene\",\
\\"availableFunds\":200000,\
\\"categoryDefaultFunding\":[\
\{\
\\"category\":\"Defense\",\
\\"amount\":1000\
\},\
\{\
\\"category\":\"Welfare\",\
\\"amount\":3000\
\},\
\{\
\\"category\":\"Science\",\
\\"amount\":5000\
\}\
\],\
\\"billSpecificFunding\":[],\
\\"caps\":[]\
\},\
\{\
\\"name\":\"Southern Palolene\",\
\\"availableFunds\":150000,\
\\"categoryDefaultFunding\":[\
\{\
\\"category\":\"Defense\",\
\\"amount\":500\
\},\
\{\
\\"category\":\"Welfare\",\
\\"amount\":5000\
\},\
\{\
\\"category\":\"Science\",\
\\"amount\":2000\
\}\
\],\
\\"billSpecificFunding\":[],\
\\"caps\":[]\
\},\
\{\
\\"name\":\"Lakos\",\
\\"availableFunds\":400000,\
\\"categoryDefaultFunding\":[\
\{\
\\"category\":\"Defense\",\
\\"amount\":10000\
\},\
\{\
\\"category\":\"Welfare\",\
\\"amount\":1000\
\},\
\{\
\\"category\":\"Science\",\
\\"amount\":500\
\}\
\],\
\\"billSpecificFunding\":[],\
\\"caps\":[]\
\}\
\]\
\}"

outputJSON = "{\"contributions\":[{\"billName\":\"An Act to Construct the Great Wall of Malodivo\",\"funds\":[{\"amount\":1000,\"district\":\"Palolene\"},{\"amount\":500,\"district\":\"Southern Palolene\"},{\"amount\":10000,\"district\":\"Lakos\"}]}]}"
