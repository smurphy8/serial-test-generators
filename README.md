# serial-test-generators

When I am programming haskell I write a lot of

``` haskell
instance ToJSON ... where
instance FromJSON ... where
instance Binary ... where

```

These libraries are often associated with state.

So, I end up writing a lot of tests of the form ...

expect (encode someTestAeson) `toBe` "{\"someSerializedThing\":\"expected encoding\"}"

so I have to write all these pieces down... but what I would really like is


``` haskell
runAesonSerializeTest 'someTestAeson "outputfile.txt"

```

That is what these libraries do for Serialize, Binary and Aeson
They make no assumption about what version of the library you are using. 


## Installation

```
$> cabal install serial-test-generators

```
