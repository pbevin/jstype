module Runtime.NumberToStringSpec where

import Test.Hspec
import Runtime.NumberToString

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

    it "goes to exponential notation for epsilon" $
      map numberToString [ 0.12345,
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
                          "0.0000012345"
                          , "1.2345e-7" ]


    it "goes to exponential notation for big numbers" $
      map numberToString [ 10000000000000000000,
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
    it "has expected values for +ve powers of 10" $ do
      splitNumber 1 `shouldBe` (1, 1, 1, 1)
      splitNumber 10 `shouldBe` (10, 2, 1, 1)
      splitNumber 100 `shouldBe` (100, 3, 1, 1)
      splitNumber 1000 `shouldBe` (1000, 4, 1, 1)

    it "has expected values for -ve powers of 10" $ do
      splitNumber 1 `shouldBe` (1, 1, 1, 1)
      splitNumber 0.1 `shouldBe` (0.1, 0, 1, 1)
      splitNumber 0.01 `shouldBe` (0.01, -1, 1, 1)
      splitNumber 0.001 `shouldBe` (0.001, -2, 1, 1)

    it "has expected values for integer precision" $ do
      splitNumber 1 `shouldBe` (1, 1, 1, 1)
      splitNumber 12 `shouldBe` (12, 2, 2, 12)
      splitNumber 123 `shouldBe` (123, 3, 3, 123)

    it "has expected values for fractional precision" $ do
      splitNumber 1 `shouldBe` (1, 1, 1, 1)
      splitNumber 1.2 `shouldBe` (1.2, 1, 2, 12)
      splitNumber 1.23 `shouldBe` (1.23, 1, 3, 123)
      splitNumber 3.1415926535897932384626 `shouldBe` (3.141592653589793, 1, 16, 3141592653589794)

    it "has expected values for multiples of 12345" $ do
      splitNumber 12345 `shouldBe` (12345, 5, 5, 12345)
      splitNumber 1234.5 `shouldBe` (1234.5, 4, 5, 12345)
      splitNumber 123.45 `shouldBe` (123.45, 3, 5, 12345)
      splitNumber 12.345 `shouldBe` (12.345, 2, 5, 12345)
      splitNumber 1.2345 `shouldBe` (1.2345, 1, 5, 12345)
      splitNumber 0.12345 `shouldBe` (0.12345, 0, 5, 12345)
      splitNumber 0.012345 `shouldBe` (0.012345, -1, 5, 12345)

  describe "findk" $ do
    it "does powers of 10" $ do
      map findk [1, 10, 100, 1000, 10000 ] `shouldBe` [1,1,1,1,1]
      map findk [0.1, 0.01, 0.001, 0.001 ] `shouldBe` [1,1,1,1]

    it "does multiples of 12345" $ do
      findk 12345 `shouldBe` 5
      map findk [12345, 1234.5, 123.45, 12.345, 1.2345] `shouldBe` [5,5,5,5,5]
      map findk [123450, 1234500, 12345000] `shouldBe` [5,5,5]
