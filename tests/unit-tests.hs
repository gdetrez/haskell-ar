import Data.Binary.Get (runGet, runGetOrFail)
import Data.Monoid ((<>))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Test.Hspec
-- import Test.QuickCheck

import Codec.Archive.Ar

main :: IO ()
main = hspec $ do

  describe "magitAsciiStr" $ do
    it "succeed with the correct string" $ do
        let myData = "abc"
        runGetOrFail (magicAsciiStr "abc") myData `shouldBe` Right ("", 3, ())
    it "fails if the input is too short" $ do
        let myData = "a"
        runGetOrFail (magicAsciiStr "abc") myData
          `shouldBe` Left (myData, 0, "demandInput: not enough bytes")
    it "fails if the input is not the right string" $ do
        let myData = "cba"
        runGetOrFail (magicAsciiStr "abc") myData
          `shouldBe` Left ("cba", 0, "Invalid magic ASCII string")

  describe "getArName" $ do
   it   "getArName returns the name of the file, stripped of excessive whitespaces" $ do
        let myData = "MYNAME          "
        runGetOrFail getArName myData `shouldBe` Right ("", 16, "MYNAME")
   it "getArName strips the / at the end of the name (GNU)" $ do
        let myData = "MYNAME/         "
        runGetOrFail getArName myData `shouldBe` Right ("", 16, "MYNAME")

  describe "getAsciiInt" $ do
   it "getAsciiInt reads an int" $ do
        let myData = "1234"
        runGetOrFail (getAsciiInt 4) myData `shouldBe` Right ("",4, 1234)

  describe "getEntry" $ do
    it "reads a simple small entry" $ do
        let myData = pad "MYNAME" 16 <> pad "12345" 12 <> pad "0" 6 <> pad "0" 6
                      <> pad "666" 8 <> pad "4" 10 <> "`\LF" <> "abcd"
        runGet getEntry myData
          `shouldBe` ("MYNAME", 12345, 0, 0, 666, "abcd")

  describe "getContent" $ do
    it "reads a simple content" $ do
        let myData = pad "8" 10 <> "`\LF" <> "abcdefgh"
        runGet getContent myData `shouldBe` "abcdefgh"

  describe "test utils" $ do
    describe "pad" $ do
      it "leave the input inchanged if no padding is required" $
        pad "abc" 0 `shouldBe` "abc"
      it "add padding if needed" $
        pad "abc" 10 `shouldBe` "abc       "

  describe "parse wot file" $ do
      it "contains the right number of parts" $ do
          file <- B.readFile "latest.wot.ar"
          let result = readAr file
          length result `shouldBe` 6
      it "yields a first part with the correct name" $ do
          file <- B.readFile "latest.wot.ar"
          let (n,_,_,_,_,_) = head $ readAr file
          n `shouldBe` "README"

-- utils
pad :: ByteString -> Int -> ByteString
pad bs n = bs <> B.replicate (fromIntegral n - B.length bs) 0x20
