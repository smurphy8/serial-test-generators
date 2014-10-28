

{- |
Module      :  Test.Serial
Description :  Test.Serial run serialization tests against static files
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable 
Portability :   non-portable (System.Posix)


-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Serial (runAesonSerializationTest, TestError (..) ) where

import           Data.Aeson (ToJSON
                           , FromJSON
                           , toJSON
                           , encode
                           , eitherDecode)


import qualified Data.ByteString.Lazy as BLazy
import           GHC.Generics
import           System.IO (withFile, IOMode(..),openFile,hIsEOF)
--------------------------------------------------

data TestError = NoFileFound |  -- NoFileFound could simply mean it is the first time the test was ran
                 AesonError String
      deriving (Generic,Read,Show,Eq,Ord)

instance ToJSON TestError where

newtype MockInference a = MockInference a
   deriving (Generic)

instance ToJSON a => ToJSON (MockInference a) where             


makeMockInference :: (ToJSON a, FromJSON a) => a -> MockInference a
makeMockInference testVal = MockInference testVal

runAesonSerializationTest :: (ToJSON a, FromJSON a) => a -> FilePath -> IO (Either TestError a)
runAesonSerializationTest dataUnderTest file = withFile file ReadWriteMode createAesonSerializeTest
 where
    createAesonSerializeTest h = do
      aNewFile <- hIsEOF h
      if aNewFile
        then writeOutputAndExit h
        else createAesonSerializeTest' h
             
    createAesonSerializeTest' h = do
      aesonByteString <- BLazy.hGetContents h
      case eitherDecode aesonByteString of
        (Left s) -> return . Left .  AesonError $ s
        (Right a) 
          |(toJSON . makeMockInference $ a) == (toJSON.makeMockInference $ dataUnderTest)
           -> return . Right $ a
          |otherwise -> return . Left . AesonError $ "Serializations do not match"
  
    writeOutputAndExit h = do
      putStrLn "file not found, writing given serialization to disk, rerun tests"
      BLazy.hPut h $ encode dataUnderTest
      return . Left $ NoFileFound

  
