module Runtime.NumberToStringSpec where

import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Runtime.NumberToString

prop_dissect :: Double -> Bool
prop_dissect d =
  let (b, e, f, p) = dissectFloat d
  in d == (fromIntegral f) * (fromIntegral b)^^(e-p)

spec :: Spec
spec = do
  describe "numberToString" $ do
    it "does simple values" $ do
      numberToString 0 `shouldBe` "0"
      numberToString 1 `shouldBe` "1"
      numberToString 6 `shouldBe` "6"
      numberToString 42 `shouldBe` "42"
      numberToString (-42) `shouldBe` "-42"
      numberToString 123 `shouldBe` "123"
      numberToString (0.1) `shouldBe` "0.1"
      numberToString (-0.1) `shouldBe` "-0.1"

    it "does special cases" $ do
      map numberToString [ 0/0, 1/0, -(1/0) ]
        `shouldBe` [ "NaN", "Infinity", "-Infinity" ]

      numberToString (encodeFloat 1 (-980)) `shouldBe` "9.785978320356312e-296"

      map numberToString [ 5.0e-3, 5.0e-4, 5.0e-5 ]
        `shouldBe` ["0.005","0.0005","0.00005"]

      map numberToString [ 2.5e-3, 2.5e-4, 2.5e-5 ]
        `shouldBe` ["0.0025","0.00025","0.000025"]

      map numberToString [ 2.5e-293, 2.5e-294, 2.5e-295 ]
        `shouldBe` [ "2.5e-293", "2.5e-294", "2.5e-295" ]

      map numberToString [ 5.0e-293, 5.0e-294, 5.0e-295 ]
        `shouldBe` [ "5e-293", "5e-294", "5e-295" ]

      numberToString 1.000000000000001
         `shouldBe` "1.000000000000001"
      numberToString 1.0000000000000001 `shouldBe` "1"

    it "goes to exponential notation for epsilon" $
      map n2s            [ 0.1234,
                           0.01234,
                           0.001234,
                           0.0001234,
                           0.00001234,
                           0.000001234,
                           0.0000001234 ]
             `shouldBe` [ "0.1234",
                          "0.01234",
                          "0.001234",
                          "0.0001234",
                          "0.00001234",
                          "0.000001234",
                          "1.234e-7" ]

    it "goes to exponential notation for epsilon" $
      map n2s            [ 0.12345,
                           0.012345,
                           0.0012345,
                           0.00012345,
                           0.000012345,
                           0.0000012345,
                           0.00000012345 ]
             `shouldBe` [ "0.12345",
                          "0.012345",
                          "0.0012345",
                          "0.00012345",
                          "0.000012345",
                          "0.0000012345",
                          "1.2345e-7" ]


    it "goes to exponential notation for big numbers" $
      map n2s            [ 10000000000000000000,
                           12345000000000000000,
                           100000000000000000000,
                           123450000000000000000,
                           1000000000000000000000,
                           1234500000000000000000 ]
        `shouldBe`      [ "10000000000000000000",
                          "12345000000000000000",
                          "100000000000000000000",
                          "123450000000000000000",
                          "1e+21",
                          "1.2345e+21" ]

  describe "splitNumber" $ do
    it "does simple values" $ do
      splitNumber 10  `shouldBe` (2, 1, [1])
      splitNumber 1   `shouldBe` (1, 1, [1])
      splitNumber 0.1 `shouldBe` (0, 1, [1])

  describe "dissectFloat" $ do
    it "satisfies d == f * b^(e-p)" $ property prop_dissect

  describe "splitNumber - n" $ do
    let splitn d = n where (n,k,s) = splitNumber d

    it "is the base10 log of the input" $ property $ \v ->
      let d = getPositive v in 1 + splitn d == splitn (d*10)
