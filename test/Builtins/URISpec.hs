module Builtins.URISpec where

import Test.Hspec
import Test.QuickCheck
import Builtins.URI

spec :: Spec
spec = do
  describe "decodeURI" $ do
    it "raises an error for invalid input" $ do
      decode "%"   `shouldBe` Nothing

    it "returns a single character that isn't %" $ do
      decode "x"   `shouldBe` Just "x"
      decode "xyz" `shouldBe` Just "xyz"

    it "decodes %xx sequences" $ do
      decode "%41"   `shouldBe` Just "A"
      decode "x%41z" `shouldBe` Just "xAz"

    it "does not decode reserved characters" $ do
      decode "%3B%2F%3F%3A%40%26%3D%2B%24%2C%23" `shouldBe` Just "%3B%2F%3F%3A%40%26%3D%2B%24%2C%23"

    it "disallows %80-%BF and %F8-%FF" $ do
      let strings = [ ['%', a, b] | a <- "89ab", b <- "0123456789abcdef" ]
                 ++ [ ['%', 'f', b] | b <- "89abcdef" ]
      map decode strings `shouldBe` replicate 72 Nothing

    it "decodes UTF8 characters" $ do
      decode "http://ru.wikipedia.org/wiki/%d0%ae%D0%bd%D0%B8%D0%BA%D0%BE%D0%B4"
        `shouldBe` Just "http://ru.wikipedia.org/wiki/Юникод"


  -- describe "binary" $ do
  --   it "decodes a number into bools" $ do
  --     binary 4 10 `shouldBe` [True, False, True, False]
  --     binary 8 10 `shouldBe` [False, False, False, False, True, False, True, False]
