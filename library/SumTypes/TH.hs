{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module SumTypes.TH
  ( -- * Constructing sum types
    constructSumType
  , SumTypeOptions
  , defaultSumTypeOptions
  , sumTypeOptionsTagOptions
  , SumTypeTagOptions (..)
  , sumTypeOptionsConstructorStrictness
  , SumTypeConstructorStrictness (..)
    -- * Converting between sum types
  , sumTypeConverter
  , partialSumTypeConverter
  ) where

import Language.Haskell.TH

-- | This is a template haskell function that creates a sum type from a list of
-- types. Here is an example:
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
constructSumType typeName SumTypeOptions{..} types = do
  let
    strictness = constructorStrictness sumTypeOptionsConstructorStrictness
    mkConstructor name =
      NormalC
      (constructorName sumTypeOptionsTagOptions typeName name)
      [(Bang NoSourceUnpackedness strictness, ConT name)]
    constructors = map mkConstructor types
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

-- | This template haskell function creates a conversion function between two
-- sum types. It works by matching up constructors that share the same inner
-- type. Note that all types in the source sum type must be present in the
-- target sum type, or you will get an error.
--
-- > data MySum
-- >   = MySumTypeA TypeA
-- >   | MySumTypeB TypeB
-- >   | MySumTypeC TypeC
-- >
-- > data OtherSum
-- >   = OtherSumTypeA TypeA
-- >   | OtherSumTypeB TypeB
-- >
-- > sumTypeConverter "otherSumToMySum" ''OtherSum ''MySum
--
-- This will producing the following code:
--
-- > otherSumToMySum :: OtherSum -> MySum
-- > otherSumToMySum (OtherSumTypeA typeA) = MySumTypeA typeA
-- > otherSumToMySum (OtherSumTypeB typeB) = MySumTypeB typeB
sumTypeConverter :: String -> Name -> Name -> Q [Dec]
sumTypeConverter functionName sourceType targetType = do
  bothConstructors <- matchTypeConstructors sourceType targetType
  let
    funcName = mkName functionName
  funcClauses <- mapM mkSerializeFunc bothConstructors
  typeDecl <- [t| $(conT sourceType) -> $(conT targetType) |]
  return
    [ SigD funcName typeDecl
    , FunD funcName funcClauses
    ]

-- | Similar to 'sumTypeConverter', except not all types in the source sum type
-- need to be present in the target sum type.
--
-- > data MySum
-- >   = MySumTypeA TypeA
-- >   | MySumTypeB TypeB
-- >   | MySumTypeC TypeC
-- >
-- > data OtherSum
-- >   = OtherSumTypeA TypeA
-- >   | OtherSumTypeB TypeB
-- >
-- > partialSumTypeConverter "mySumToOtherSum" ''MySum ''OtherSum
--
-- This will producing the following code:
--
-- > mySumToOtherSum :: MySum -> Maybe OtherSum
-- > mySumToOtherSum (MySumTypeA typeA) = Just $ OtherSumTypeA typeA
-- > mySumToOtherSum (MySumTypeB typeB) = Just $ OtherSumTypeB typeB
-- > mySumToOtherSum other = Nothing
partialSumTypeConverter :: String -> Name -> Name -> Q [Dec]
partialSumTypeConverter functionName sourceType targetType = do
  bothConstructors <- matchTypeConstructors targetType sourceType
  let
    funcName = mkName functionName
    wildcardClause = Clause [WildP] (NormalB (ConE 'Nothing)) []
  funcClauses <- mapM mkDeserializeFunc bothConstructors
  typeDecl <- [t| $(conT sourceType) -> Maybe $(conT targetType) |]

  return
    [ SigD funcName typeDecl
    , FunD funcName (funcClauses ++ [wildcardClause])
    ]

matchTypeConstructors :: Name -> Name -> Q [BothConstructors]
matchTypeConstructors sourceType targetType = do
  sourceConstructors <- typeConstructors sourceType
  targetConstructors <- typeConstructors targetType
  mapM (matchConstructor targetConstructors) sourceConstructors

-- | Extract the constructors and types for the given sum type.
typeConstructors :: Name -> Q [(Type, Name)]
typeConstructors typeName = do
  info <- reify typeName
  case info of
    (TyConI (DataD _ _ _ _ constructors _)) -> mapM go constructors
      where
        go (NormalC name []) = fail $ "Constructor " ++ nameBase name ++ " doesn't have any arguments"
        go (NormalC name [(_, type')]) = return (type', name)
        go (NormalC name _) = fail $ "Constructor " ++ nameBase name ++ " has more than one argument"
        go _ = fail $ "Invalid constructor in " ++ nameBase typeName
    _ -> fail $ nameBase typeName ++ " must be a sum type"

-- | Find the corresponding target constructor for a given source constructor.
matchConstructor :: [(Type, Name)] -> (Type, Name) -> Q BothConstructors
matchConstructor targetConstructors (type', sourceConstructor) = do
  targetConstructor <-
    maybe
    (fail $ "Can't find constructor in target type corresponding to " ++ nameBase sourceConstructor)
    return
    (lookup type' targetConstructors)
  return $ BothConstructors type' sourceConstructor targetConstructor

-- | Utility type to hold the source and target constructors for a given type.
data BothConstructors =
  BothConstructors
  { innerType :: Type
  , sourceConstructor :: Name
  , targetConstructor :: Name
  }

-- | Construct the TH function 'Clause' for the serialization function for a
-- given type.
mkSerializeFunc :: BothConstructors -> Q Clause
mkSerializeFunc BothConstructors{..} = do
  varName <- newName "value"
  let
    patternMatch = ConP sourceConstructor [VarP varName]
    constructor = AppE (ConE targetConstructor) (VarE varName)
  return $ Clause [patternMatch] (NormalB constructor) []

-- | Construct the TH function 'Clause' for the deserialization function for a
-- given type.
mkDeserializeFunc :: BothConstructors -> Q Clause
mkDeserializeFunc BothConstructors{..} = do
  varName <- newName "value"
  let
    patternMatch = ConP targetConstructor [VarP varName]
    constructor = AppE (ConE 'Just) (AppE (ConE sourceConstructor) (VarE varName))
  return $ Clause [patternMatch] (NormalB constructor) []
