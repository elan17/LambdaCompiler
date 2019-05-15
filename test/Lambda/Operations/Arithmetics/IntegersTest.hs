module Lambda.Operations.Arithmetics.IntegersTest where

import  Lambda
import Lambda.Operations.Arithmetics.Integers as D

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import           Test.QuickCheck
import           Test.Hspec

newtype LittleInt = LittleInt Integer deriving (Show)

prop_Suma :: LittleInt -> LittleInt -> Bool
prop_Suma (LittleInt n1) (LittleInt n2) =
    (fromInteger n1 :: LambdaTerm) + fromInteger n2 == fromInteger (n1+n2)

prop_Mul :: LittleInt -> LittleInt -> Bool
prop_Mul (LittleInt n1) (LittleInt n2) =
    (fromInteger n1 :: LambdaTerm) * fromInteger n2 == fromInteger (n1*n2)

prop_integerParser :: LittleInt -> Bool
prop_integerParser (LittleInt n1) =
    D.toInteger (fromInteger n1) == Just n1

instance Arbitrary LittleInt where
    arbitrary = oneof $ fmap (return . LittleInt) [0..25]

integersTest =
    describe "Lambda.Operations.Arithmetics.Integers" $ do
    it "Comprobando suma" $
        property prop_Suma
    it "Comprobando multiplicación" $
        property prop_Mul
    it "Comprobando el parseo de integers" $
        property prop_integerParser
