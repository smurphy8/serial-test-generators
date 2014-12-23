{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{- |
Module      :  Test.Serial
Description :  Test.Serial run serialization tests against static files
Copyright   :  (c) Plow Technologies
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  unstable 
Portability :   non-portable (System.Posix)


-}


module Test.Serial (runAesonSerializationTest
                   , runBinarySerializationTest
                   , runCerealSerializationTest
                   , TestError (..) ) where

import   qualified Data.Aeson as A
import   qualified Data.Binary as B
import   qualified Data.Serialize as C
import qualified Data.ByteString as BStrict
import qualified Data.ByteString.Lazy as BLazy
import           GHC.Generics
import Data.String.Here.Interpolated (i)
import           System.IO (withFile, IOMode(..),hIsEOF)
--------------------------------------------------

data TestError = NoFileFound |  -- NoFileFound could simply mean it is the first time the test was ran
                 AesonError  String |
                 BinaryError String |
                 CerealError String                  
      deriving (Generic,Read,Show,Eq,Ord)

instance A.ToJSON TestError where



-- | AESON SERIALIZER  
-- | 'MockAesonInference' just to force two inferred types to be the same  
newtype MockAesonInference a = MockAesonInference a
   deriving (Generic)

instance A.ToJSON a => A.ToJSON (MockAesonInference a) where             


makeMockAesonInference :: (A.ToJSON a, A.FromJSON a ) => a -> MockAesonInference a
makeMockAesonInference testVal = MockAesonInference testVal

runAesonSerializationTest :: (A.ToJSON a, A.FromJSON a) => a -> FilePath -> IO (Either TestError a)
runAesonSerializationTest dataUnderTest file = withFile file ReadWriteMode createAesonSerializeTest
 where
    createAesonSerializeTest h = do
      aNewFile <- hIsEOF h
      if aNewFile
        then writeOutputAndExit h
        else createAesonSerializeTest' h
             
    createAesonSerializeTest' h = do
      aesonByteString <- BLazy.hGetContents h
      case A.eitherDecode aesonByteString of
        (Left s) -> return . Left .  AesonError $ s
        (Right a) 
          |(A.toJSON . makeMockAesonInference $ a) == (A.toJSON.makeMockAesonInference $ dataUnderTest)
           -> return . Right $ a
          |otherwise -> return . Left . AesonError . explainError (A.toJSON . makeMockAesonInference $ a) $ (A.toJSON . makeMockAesonInference $ dataUnderTest)

    writeOutputAndExit h = do
      putStrLn "file not found, writing given serialization to disk, rerun tests"
      BLazy.hPut h $ A.encode dataUnderTest
      return . Left $ NoFileFound

explainError old new = [i|
  JSON doesn't match: 
   old:  ${old}
   new:  ${new}
  |]




-- | 'MockBinaryInference' just to force two inferred types to be the same  
newtype MockBinaryInference a = MockBinaryInference a
   deriving (Generic)

instance B.Binary a => B.Binary (MockBinaryInference a) where             


makeMockBinaryInference :: (B.Binary a) => a -> MockBinaryInference a
makeMockBinaryInference testVal = MockBinaryInference testVal

runBinarySerializationTest :: (B.Binary a) => a -> FilePath -> IO (Either TestError a)
runBinarySerializationTest dataUnderTest file = withFile file ReadWriteMode createBinarySerializeTest
 where
    createBinarySerializeTest h = do
      aNewFile <- hIsEOF h
      if aNewFile
        then writeOutputAndExit h
        else createBinarySerializeTest' h
             
    createBinarySerializeTest' h = do
      binaryByteString <- BLazy.hGetContents h
      case B.decodeOrFail binaryByteString of
        (Left s) -> return . Left .  BinaryError . show $ s
        (Right (_,_,a) )
          |(B.encode . makeMockBinaryInference $ a) == (B.encode.makeMockBinaryInference $ dataUnderTest)
           -> return . Right $ a
          |otherwise -> return . Left . BinaryError $ "Serializations do not match"
  
    writeOutputAndExit h = do
      putStrLn "file not found, writing given serialization to disk, rerun tests"
      BLazy.hPut h $ B.encode dataUnderTest
      return . Left $ NoFileFound






-- | 'MockCerealInference' just to force two inferred types to be the same  
newtype MockCerealInference a = MockCerealInference a
   deriving (Generic)

instance C.Serialize a => C.Serialize (MockCerealInference a) where             


makeMockCerealInference :: (C.Serialize a) => a -> MockCerealInference a
makeMockCerealInference testVal = MockCerealInference testVal

runCerealSerializationTest :: (C.Serialize a) => a -> FilePath -> IO (Either TestError a)
runCerealSerializationTest dataUnderTest file = withFile file ReadWriteMode createCerealSerializeTest
 where
    createCerealSerializeTest h = do
      aNewFile <- hIsEOF h
      if aNewFile
        then writeOutputAndExit h
        else createCerealSerializeTest' h
             
    createCerealSerializeTest' h = do
      cerealByteString <- BStrict.hGetContents h
      case C.decode cerealByteString of
        (Left s) -> return . Left .  CerealError $ s
        (Right a)
          |(C.encode . makeMockCerealInference $ a) == (C.encode.makeMockCerealInference $ dataUnderTest)
           -> return . Right $ a
          |otherwise -> return . Left . CerealError $ "Serializations do not match"
  
    writeOutputAndExit h = do
      putStrLn "file not found, writing given serialization to disk, rerun tests"
      BStrict.hPut h $ C.encode dataUnderTest
      return . Left $ NoFileFound

