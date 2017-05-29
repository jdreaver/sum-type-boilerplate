{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module SumTypes.THSpec (spec) where

import Test.Hspec

import SumTypes.TH

data TypeA = TypeA deriving (Show, Eq)
data TypeB = TypeB deriving (Show, Eq)
data TypeC = TypeC deriving (Show, Eq)

constructSumType "MySum" defaultSumTypeOptions [''TypeA, ''TypeB, ''TypeC]

deriving instance Show MySum
deriving instance Eq MySum

spec :: Spec
spec = do
  describe "mkSumType" $ do
    it "can create events" $ do
      -- The real utility in this test is the fact that it compiles
      length [MySumTypeA TypeA, MySumTypeB TypeB, MySumTypeC TypeC] `shouldBe` 3
