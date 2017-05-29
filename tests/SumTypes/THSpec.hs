{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module SumTypes.THSpec (spec) where

import Test.Hspec

import SumTypes.TH

data TypeA = TypeA deriving (Show, Eq)
data TypeB = TypeB deriving (Show, Eq)
data TypeC = TypeC deriving (Show, Eq)

constructSumType "MySum" defaultSumTypeOptions [''TypeA, ''TypeB, ''TypeC]
constructSumType "OtherSum" defaultSumTypeOptions [''TypeA, ''TypeB]
sumTypeConverter "otherSumToMySum" ''OtherSum ''MySum
partialSumTypeConverter "mySumToOtherSum" ''MySum ''OtherSum

deriving instance Show MySum
deriving instance Eq MySum

deriving instance Show OtherSum
deriving instance Eq OtherSum

spec :: Spec
spec = do
  describe "constructSumType" $ do
    it "compiles without breaking" $ do
      -- The real utility in this test is the fact that it compiles
      length [MySumTypeA TypeA, MySumTypeB TypeB, MySumTypeC TypeC] `shouldBe` 3

  describe "sumTypeConverter" $ do
    it "properly converts between types" $ do
      otherSumToMySum (OtherSumTypeA TypeA) `shouldBe` MySumTypeA TypeA
      otherSumToMySum (OtherSumTypeB TypeB) `shouldBe` MySumTypeB TypeB

  describe "partialSumTypeConverter" $ do
    it "properly converts between types" $ do
      mySumToOtherSum (MySumTypeA TypeA) `shouldBe` Just (OtherSumTypeA TypeA)
      mySumToOtherSum (MySumTypeB TypeB) `shouldBe` Just (OtherSumTypeB TypeB)
      mySumToOtherSum (MySumTypeC TypeC) `shouldBe` Nothing
