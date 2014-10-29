{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SerialSpec (main, spec) where

import Control.Applicative
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Test.Hspec
import Test.QuickCheck.Monadic
import Test.QuickCheck
import Test.Serial ( runAesonSerializationTest
                   , runBinarySerializationTest
                   , TestError (..) )
import Filesystem 

data TestSerialData = TestSerialData {
                             anExampleStringField :: String,
                             anExampleIntField    :: Int,
                             anExampleListTupleField   :: (Int,Int)
                             }

                      deriving (Generic,Eq)

testAesonSerialFilePath = "serialfiles/aesontest.json"

instance ToJSON  TestSerialData where
instance FromJSON  TestSerialData where   


generateTestSerialData ::  [Int] -> [String] -> [(Int, Int)] -> [TestSerialData]
generateTestSerialData integers strings integerTuples = TestSerialData <$> strings <*> integers <*> integerTuples




quickCheckAesonSerializeIsIsoMorphic integers strings integerTuples = do
  let testData = generateTestSerialData integers strings integerTuples
  eTestData <- liftIO $ runAesonSerializationTest testData testAesonSerialFilePath
  case eTestData of
    (Left NoFileFound) -> do
      eTestNowThatFileIsMade <- liftIO $ runAesonSerializationTest testData testAesonSerialFilePath
      liftIO $ removeFile "serialfiles/aesontest.json"
      assert (eTestNowThatFileIsMade == (Right testData))
    _ -> assert (False == True)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should serialize and deserialize correctly " $ property $ (\s i t -> monadicIO $ quickCheckAesonSerializeIsIsoMorphic s i t)



testData = generateTestSerialData [0] [""] [(0,0)]

