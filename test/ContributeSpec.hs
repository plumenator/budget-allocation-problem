{-# LANGUAGE OverloadedStrings #-}

module ContributeSpec where

import Data.Map.Strict (empty, fromList)
import Data.Ratio
import Test.Hspec

import Contribute
import Model


spec :: Spec
spec = do
  testBillAllocationProportion
  testBillAvailableFunds
  oneBillOneDistrict
  oneBillMultipleDistricts
  taxShortFall
  taxExcess

testBillAllocationProportion :: Spec
testBillAllocationProportion = do
  describe "Calculate the allocation portion of a Bill" $ do
    it "returns zero when there are no category default" $
      billAllocationProportion Defense District { districtName = DistrictName "Tulsa",
                                                 availableFunds = Amount 10000,
                                                 categoryDefaultFunding = empty
                                               } `shouldBe` 0
    it "returns zero when there are no default for that category" $
      billAllocationProportion Defense District { districtName = DistrictName "Tulsa",
                                                 availableFunds = Amount 10000,
                                                 categoryDefaultFunding = fromList [(Welfare, Amount 10)]
                                               } `shouldBe` 0
    it "returns ratio when there are defaults for multiple categories" $
      billAllocationProportion Defense District { districtName = DistrictName "Tulsa",
                                                 availableFunds = Amount 10000,
                                                 categoryDefaultFunding = fromList [(Welfare, Amount 10), (Science, Amount 10), (Defense, Amount 10)]
                                               } `shouldBe` (1 % 3)

testBillAvailableFunds :: Spec
testBillAvailableFunds = do
  describe "Calculate the available funds a Bill given a District" $ do
    it "returns zero when there are no category default" $
      billAvailableFunds Defense District { districtName = DistrictName "Tulsa",
                                            availableFunds = Amount 10000,
                                            categoryDefaultFunding = empty
                                          } `shouldBe` Amount 0
    it "returns zero when there are no default for that category" $
      billAvailableFunds Defense District { districtName = DistrictName "Tulsa",
                                            availableFunds = Amount 10000,
                                            categoryDefaultFunding = fromList [(Welfare, Amount 10)]
                                          } `shouldBe` Amount 0
    it "returns all allocated funds when allocated < available proportion" $
      billAvailableFunds Defense District { districtName = DistrictName "Tulsa",
                                            availableFunds = Amount 10000,
                                            categoryDefaultFunding = fromList [(Welfare, Amount 10), (Science, Amount 10), (Defense, Amount 10)]
                                          } `shouldBe` Amount 10
    it "returns all available proportion when available proportion <= allocated" $
      billAvailableFunds Defense District { districtName = DistrictName "Tulsa",
                                            availableFunds = Amount 10000,
                                            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 10000), (Defense, Amount 10000)]
                                          } `shouldBe` Amount 3333

oneBillOneDistrict :: Spec
oneBillOneDistrict = do
  describe "Contribute funds from one District to one Bill" $ do
    it "contributes no funds when no default is provided" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = empty
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 0
                                                     }]
                                       }
    it "contributes all allocated funds when a default is provided only for that category" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 10
                                                     }]
                                       }
    it "contributes contribution proportion when a default is provided for multiple categories" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 10000), (Defense, Amount 10000)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 3333
                                                     }]
                                       }

oneBillMultipleDistricts :: Spec
oneBillMultipleDistricts = do
  describe "Contribute funds from multiple Districts to one Bill" $ do
    it "contributes no funds when no default is provided" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 0
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 0
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 0
                                                     }]
                                       }
    it "contributes all allocated funds when a default is provided only for that district" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 10
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 0
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 0
                                                     }]
                                       }
    it "contributes contribution proportion when a default is provided only for multiple districts" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)]
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 10
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 5
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 5
                                                     }]
                                       }

taxShortFall :: Spec
taxShortFall = do
  describe "Contribute funds when available < required" $ do
    it "contributes no funds when no funds are available" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 0,
            categoryDefaultFunding = fromList [(Defense, Amount 10)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 0,
            categoryDefaultFunding = fromList [(Defense, Amount 5)]
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 0,
            categoryDefaultFunding = fromList [(Defense, Amount 5)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 0
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 0
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 0
                                                     }]
                                       }
    it "contributes allocation proportion when fewer funds are available" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 100,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 100
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 50
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 50
                                                     }]
                                       }

taxExcess :: Spec
taxExcess = do
  describe "Contribute funds when available > required" $ do
    it "contributes contribution proportion when one district can fund the bill alone" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 199900
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 49
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 49
                                                     }]
                                       }
    it "contributes contribution proportion when one district falls short " $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 99987
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 24
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 99987
                                                     }]
                                       }
    it "contributes contribution proportion when available/allocated >> required" $
      contribute Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 2000
      } [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 3000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 1000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)]
            }] `shouldBe` Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 1960
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 29
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 9
                                                     }]
                                       }
