module Operations.Arithmetics.IntegersTest where

import CompiladorLambda
import Operations.Arithmetics.Integers

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype LittleInt = LittleInt Integer deriving (Show)

prop_Suma :: LittleInt -> LittleInt -> Bool
prop_Suma (LittleInt n1) (LittleInt n2) =
    (fromInteger n1 :: LambdaTerm) + fromInteger n2 == fromInteger (n1+n2)

prop_Mul :: LittleInt -> LittleInt -> Bool
prop_Mul (LittleInt n1) (LittleInt n2) =
        (fromInteger n1 :: LambdaTerm) * fromInteger n2 == fromInteger (n1*n2)

instance Arbitrary LittleInt where
    arbitrary = oneof $ fmap (return . LittleInt) [0..25]
