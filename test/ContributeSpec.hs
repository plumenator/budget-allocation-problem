{-# LANGUAGE OverloadedStrings #-}

module ContributeSpec where

import Data.Map.Strict (empty, fromList)
import Data.Ratio
import Test.Hspec

import Contribute
import Model


spec :: Spec
spec = do
  oneBillOneDistrict
  multipleBillsOneDistrict
  oneBillMultipleDistricts
  multipleBillsMultipleDistricts
  taxShortFall
  taxExcess

oneBillOneDistrict :: Spec
oneBillOneDistrict = do
  describe "Contribute funds from one District to one Bill" $ do
    it "contributes no funds when no default is provided" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = empty,
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 0
                                                     }]
                                       }]
    it "contributes all allocated funds when a default is provided only for that category" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 10
                                                     }]
                                       }]
    it "contributes contribution proportion when a default is provided for multiple categories" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 10000), (Defense, Amount 10000)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 10000
                                                     }]
                                       }]
    it "contributes contribution proportion when a bill specific is provided" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 10000), (Defense, Amount 10000)],
            billSpecificFunding = fromList [(BillName "A bill", Amount 500)],
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 500
                                                     }]
                                       }]

multipleBillsOneDistrict :: Spec
multipleBillsOneDistrict = do
  describe "Contribute funds from one District to multiple Bills" $ do
    it "contributes no funds when no default is provided" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = empty,
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 0
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 0
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 0
                                                                      }]
                                                        }]
    it "contributes proportionally when a default is provided for all categories" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 5000), (Defense, Amount 5000)],
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 5000
                                                                      }]
                                                        }]
    it "contributes proportionally when a bill specific is provided" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 5000), (Defense, Amount 5000)],
                            billSpecificFunding = fromList [(BillName "A bill", Amount 500)],
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 322
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 3225
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 6451
                                                                      }]
                                                        }]
    it "contributes proportionally when a category cap is provided" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 5000), (Defense, Amount 5000)],
                            billSpecificFunding = empty,
                            caps = fromList [(Defense, Amount 500)]
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 322
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 3225
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 6451
                                                                      }]
                                                        }]
    it "contributes proportionally when a category cap is provided for a category with two bills" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                  Bill {
                     Model.billName = BillName "The same category",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Welfare, Amount 10000), (Science, Amount 5000), (Defense, Amount 5000)],
                            billSpecificFunding = empty,
                            caps = fromList [(Defense, Amount 500)]
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 161
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "The same category",
                                                           funds = [Fund {
                                                                       district = DistrictName "Tulsa",
                                                                       Contribute.amount = Amount 161
                                                                       }]
                                                         },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 3225
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 6451
                                                                      }]
                                                        }]

oneBillMultipleDistricts :: Spec
oneBillMultipleDistricts = do
  describe "Contribute funds from multiple Districts to one Bill" $ do
    it "contributes no funds when no default is provided" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = empty,
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty,
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty,
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
    it "contributes all allocated funds when a default is provided only for that district" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty,
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = empty,
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
    it "contributes contribution proportion when a default is provided only for multiple districts" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
    it "contributes contribution proportion when a bill specific is provided" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 10)],
            billSpecificFunding = fromList [(BillName "A bill", Amount 500)],
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 500
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 5
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 5
                                                     }]
                                       }]
    it "contributes contribution proportion when a category cap is provided" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 10000,
            categoryDefaultFunding = fromList [(Defense, Amount 1000000)],
            billSpecificFunding = empty,
            caps = fromList [(Defense, Amount 500)]
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 5000,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                         funds = [Fund {
                                                     district = DistrictName "Tulsa",
                                                     Contribute.amount = Amount 500
                                                     },
                                                   Fund {
                                                     district = DistrictName "Idaho",
                                                     Contribute.amount = Amount 5
                                                     },
                                                   Fund {
                                                     district = DistrictName "Attica",
                                                     Contribute.amount = Amount 5
                                                     }]
                                       }]

multipleBillsMultipleDistricts :: Spec
multipleBillsMultipleDistricts = do
  describe "Contribute funds from multiple Districts to multiple Bills" $ do
    it "contributes no funds when no default is provided" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = empty,
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Idaho",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = empty,
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Attica",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = empty,
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
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
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
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
                                                        }]
    it "contributes all allocated funds when each district sponsors one category" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Idaho",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Science, Amount 200000)],
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Attica",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Welfare, Amount 200000)],
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 10000
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 0
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 0
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 0
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 5000
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 0
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
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
                                                                      Contribute.amount = Amount 5000
                                                                      }]
                                                        }]
    it "contributes proportionally when all districts sponsor all categories" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Defense, Amount 5000), (Science, Amount 2500), (Welfare, Amount 2500)],
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Idaho",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Defense, Amount 1250), (Science, Amount 2500), (Welfare, Amount 1250)],
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Attica",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Defense, Amount 1250), (Science, Amount 1250), (Welfare, Amount 2500)],
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 5000
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 1250
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 1250
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 1250
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 1250
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 2500
                                                                      }]
                                                        }]
    it "contributes proportionally when all districts sponsor all categories and there's a bill specific" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Defense, Amount 5000), (Science, Amount 2500), (Welfare, Amount 2500)],
                            billSpecificFunding = fromList [(BillName "A bill", Amount 500)],
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Idaho",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Defense, Amount 1250), (Science, Amount 2500), (Welfare, Amount 1250)],
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Attica",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Defense, Amount 1250), (Science, Amount 1250), (Welfare, Amount 2500)],
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 1250
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 1250
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 1250
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 1250
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 2500
                                                                      }]
                                                        }]
    it "contributes proportionally when all districts sponsor all categories and there's a category cap" $
      contribute [Bill {
                     Model.billName = BillName "A bill",
                     category = Defense,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Another bill",
                     category = Science,
                     Model.amount = Amount 200000
                     },
                   Bill {
                     Model.billName = BillName "Yet another bill",
                     category = Welfare,
                     Model.amount = Amount 200000
                     }] [District {
                            districtName = DistrictName "Tulsa",
                            availableFunds = Amount 10000,
                            categoryDefaultFunding = fromList [(Defense, Amount 5000), (Science, Amount 2500), (Welfare, Amount 2500)],
                            billSpecificFunding = empty,
                            caps = fromList [(Defense, Amount 500)]
                            },
                         District {
                            districtName = DistrictName "Idaho",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Defense, Amount 1250), (Science, Amount 2500), (Welfare, Amount 1250)],
                            billSpecificFunding = empty,
                            caps = empty
                            },
                         District {
                            districtName = DistrictName "Attica",
                            availableFunds = Amount 5000,
                            categoryDefaultFunding = fromList [(Defense, Amount 1250), (Science, Amount 1250), (Welfare, Amount 2500)],
                            billSpecificFunding = empty,
                            caps = empty
                            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 1250
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 1250
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 1250
                                                                      }]
                                                        },
                                            Contribution { Contribute.billName = BillName "Yet another bill",
                                                          funds = [Fund {
                                                                      district = DistrictName "Tulsa",
                                                                      Contribute.amount = Amount 2500
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Idaho",
                                                                      Contribute.amount = Amount 1250
                                                                      },
                                                                   Fund {
                                                                      district = DistrictName "Attica",
                                                                      Contribute.amount = Amount 2500
                                                                      }]
                                                        }]


taxShortFall :: Spec
taxShortFall = do
  describe "Contribute funds when available < required" $ do
    it "contributes no funds when no funds are available" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 0,
            categoryDefaultFunding = fromList [(Defense, Amount 10)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 0,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 0,
            categoryDefaultFunding = fromList [(Defense, Amount 5)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
    it "contributes allocation proportion when fewer funds are available" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 100,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]

taxExcess :: Spec
taxExcess = do
  describe "Contribute funds when available > required" $ do
    it "contributes contribution proportion when one district can fund the bill alone" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
    it "contributes contribution proportion when one district falls short " $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 200000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 50,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
    it "contributes contribution proportion when available/allocated >> required" $
      contribute [Bill {
      Model.billName = BillName "A bill",
      category = Defense,
      Model.amount = Amount 2000
      }] [District {
            districtName = DistrictName "Tulsa",
            availableFunds = Amount 200000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Idaho",
            availableFunds = Amount 3000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            },
          District {
            districtName = DistrictName "Attica",
            availableFunds = Amount 1000,
            categoryDefaultFunding = fromList [(Defense, Amount 200000)],
            billSpecificFunding = empty,
            caps = empty
            }] `shouldBe` [Contribution { Contribute.billName = BillName "A bill",
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
                                       }]
