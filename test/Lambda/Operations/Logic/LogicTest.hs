module Lambda.Operations.Logic.LogicTest where

import           Lambda
import           Lambda.Operations.Logic
import           Lambda.DefaultBindings

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Test.QuickCheck
import           Test.Hspec

newtype BooleanLambda = BooleanLambda LambdaTerm deriving (Show)

-- Es un test, no requiere eficiencia
prop_and :: BooleanLambda -> BooleanLambda -> Bool
prop_and (BooleanLambda x) (BooleanLambda y)
    | x == falso Prelude.|| y == falso = f == falso
    | otherwise                = f == verdadero
    where f = reduceDefault (puertaAND <\ x <\ y)

prop_or :: BooleanLambda -> BooleanLambda -> Bool
prop_or (BooleanLambda x) (BooleanLambda y)
    | x == verdadero Prelude.|| y == verdadero = f == verdadero
    | otherwise                        = f == falso
    where f = reduceDefault (puertaOR  <\ x <\ y)

prop_not :: BooleanLambda -> Bool
prop_not (BooleanLambda x) | x == verdadero = f == falso
                           | otherwise      = f == verdadero
    where f = reduceDefault (puertaNOT  <\ x)

prop_parser ::  Bool -> Bool
prop_parser x = (toBoolean . fromBoolean) x == Just x

instance Arbitrary BooleanLambda where
    arbitrary = BooleanLambda <$> oneof [return falso, return verdadero]

logicTest =
    describe "Lambda.Operations.Arithmetics.Logic" $ do
    it "Comprobando operador and" $
        property  prop_and
    it "Comprobando operador or" $
        property prop_or
    it "Comprobando operador not" $
        property prop_not
    it "Comprobando parseo de booleados" $
        property prop_parser
