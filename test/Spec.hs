import Test.Hspec (hspec, describe, it, shouldBe)

import Data.Typeable (typeOf)
import Lib (toUpperCase, frequency, numTimesFound, randomString, atLeastNtimes)

main :: IO ()
main = hspec $ do
    describe "toUpperCase" $ do
        it "Should return upper case string" $ do
            toUpperCase "haskell" `shouldBe` "HASKELL"
    
    describe "frequency" $ do
        it "Should return frequency of every letter" $ do
            frequency "HASKELL" `shouldBe` [('A',1),('E',1),('H',1),('K',1),('L',2),('S',1)]

    describe "numTimesFound" $ do
        it "Should return frequency of a given letter" $ do
            numTimesFound 'L' "HASKELL" `shouldBe` 2

    describe "randomString" $ do
        it "Should return String" $ do
            randomStr <- randomString
            typeOf randomStr `shouldBe` typeOf "haskell"
        it "Should return string of 10 letters" $ do
            randomStr <- randomString
            length randomStr `shouldBe` 10
    
    describe "atLeastNtimes" $ do
        it "Should return boolean" $ do
            atLeastNtimes 2 "HASKELL" 'L' `shouldBe` True
