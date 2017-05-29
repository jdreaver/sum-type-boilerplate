{-# LANGUAGE RecordWildCards #-}

module SumTypes.TH
  ( constructSumType
  , SumTypeOptions
  , defaultSumTypeOptions
  , sumTypeOptionsTagOptions
  , SumTypeTagOptions (..)
  , sumTypeOptionsConstructorStrictness
  , SumTypeConstructorStrictness (..)
  ) where

import Language.Haskell.TH

-- | This is a template haskell function that creates a sum type from a list of
-- types. This is very useful when creating an event type for a 'Projection'
-- from a list of events in your system. Here is an example:
--
-- > data TypeA = TypeA
-- > data TypeB = TypeB
-- > data TypeC = TypeC
-- >
-- > constructSumType "MySum" defaultSumTypeOptions [''TypeA, ''TypeB, ''TypeC]
--
-- This will produce the following sum type:
--
-- > data MySum
-- >   = MySumTypeA TypeA
-- >   | MySumTypeB TypeB
-- >   | MySumTypeC TypeC
--
-- Note that you can use standalone deriving to derive any instances you want:
--
-- > deriving instance Show MySum
-- > deriving instance Eq MySum
constructSumType :: String -> SumTypeOptions -> [Name] -> Q [Dec]
constructSumType typeName SumTypeOptions{..} eventTypes = do
  let
    strictness = constructorStrictness sumTypeOptionsConstructorStrictness
    mkConstructor eventName =
      NormalC
      (constructorName sumTypeOptionsTagOptions typeName eventName)
      [(Bang NoSourceUnpackedness strictness, ConT eventName)]
    constructors = map mkConstructor eventTypes
  return [DataD [] (mkName typeName) [] Nothing constructors []]

-- | Options for 'constructSumType'. Note that the constructor for this type is
-- not exported, please use 'defaultSumTypeOptions'. (This is done for
-- the sake of backwards compatibility in case we add options.)
data SumTypeOptions
  = SumTypeOptions
  { sumTypeOptionsTagOptions :: SumTypeTagOptions
  , sumTypeOptionsConstructorStrictness :: SumTypeConstructorStrictness
  }

-- | Default options for 'SumTypeOptions'
--
-- @
-- 'SumTypeOptions'
-- { 'sumTypeOptionsTagOptions' = 'PrefixTagsWithTypeName'
-- , 'sumTypeOptionsConstructorStrictness' = 'LazySumTypeConstructors'
-- }
-- @
defaultSumTypeOptions :: SumTypeOptions
defaultSumTypeOptions =
  SumTypeOptions
  { sumTypeOptionsTagOptions = PrefixTagsWithTypeName
  , sumTypeOptionsConstructorStrictness = LazySumTypeConstructors
  }

-- | This type specifies how 'constructSumType' will generate the tags for each
-- type.
data SumTypeTagOptions
  = PrefixTagsWithTypeName
    -- ^ This option generates tags with the sum type name prefixed to each
    -- tag.
  | AppendTypeNameToTags
    -- ^ This option generates tags with the sum type name appended to each
    -- tag.
  | ConstructTagName (String -> String)
    -- ^ Uses the given function to construct an arbitrary tag name. The
    -- argument to this function is the name of the tagged type.

constructorName :: SumTypeTagOptions -> String -> Name -> Name
constructorName PrefixTagsWithTypeName typeName = mkName . (typeName ++) . nameBase
constructorName AppendTypeNameToTags typeName = mkName . (++ typeName) . nameBase
constructorName (ConstructTagName mkConstructor) _ = mkName . mkConstructor . nameBase

-- | Defines if the constructors for the sum type should be lazy or strict.
data SumTypeConstructorStrictness
  = LazySumTypeConstructors
    -- ^ Constructors will be lazy
  | StrictSumTypeConstructors
    -- ^ Constructors will be strict
  deriving (Show, Eq)

constructorStrictness :: SumTypeConstructorStrictness -> SourceStrictness
constructorStrictness LazySumTypeConstructors = NoSourceStrictness
constructorStrictness StrictSumTypeConstructors = SourceStrict
