# sum-type-boilerplate: A Haskell sum type library

This library allows users to use Template Haskell to easily construct and
manipulate sum types.

## Sum Types

A sum type (also called
a [tagged union](https://en.wikipedia.org/wiki/Tagged_union)) looks like this
in Haskell:

```haskell
data MySum
  = MySumTypeA TypeA
  | MySumTypeB TypeB
  | MySumTypeC TypeC
```

Constructing this when you have a large number of types can be error-prone if
you intend to have a uniform naming scheme, and if you intend to only have each
type present once.

If you have many sum types with overlapping sets of types, then functions to
convert between them can be full of boilerplate.

```haskell
data OtherSum
  = OtherSumTypeA TypeA
  | OtherSumTypeB TypeB

otherSumToMySum :: OtherSum -> MySum
otherSumToMySum (OtherSumTypeA typeA) = MySumTypeA typeA
otherSumToMySum (OtherSumTypeB typeB) = MySumTypeB typeB

mySumToOtherSum :: MySum -> Maybe OtherSum
mySumToOtherSum (MySumTypeA typeA) = Just $ OtherSumTypeA typeA
mySumToOtherSum (MySumTypeB typeB) = Just $ OtherSumTypeB typeB
mySumToOtherSum other = Nothing
```

The boilerplate in examples isn't too bad, but consider writing this when your
sum type has dozens of types in it!

## Where does this library come in?

Using `sum-type-boilerplate`, you can reduce all of the previous code into the
following:

```haskell
constructSumType "MySum" defaultSumTypeOptions [''TypeA, ''TypeB, ''TypeC]
constructSumType "OtherSum" defaultSumTypeOptions [''TypeA, ''TypeB]
sumTypeConverter "otherSumToMySum" ''OtherSum ''MySum
partialSumTypeConverter "mySumToOtherSum" ''MySum ''OtherSum
```

## More features

* This library has an extremely simple implementation. (In fact, it is useful
  as an example if you are learning `template-haskell` for the first time.)
* The only dependency is on `template-haskell`. That means adding it as a
  dependency probably doesn't introduce transitive dependencies.
* The template haskell used here just produces vanilla Haskell data types. No
  crazy type-level magic is going on. That means if you want to ditch this
  library later on, just copy the generated code into your project.
