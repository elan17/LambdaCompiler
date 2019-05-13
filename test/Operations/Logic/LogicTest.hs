module Operations.Logic.LogicTest where

import           CompiladorLambda
import           Operations.Logic

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

newtype BooleanLambda = BooleanLambda LambdaTerm deriving (Show)

-- Es un test, no requiere eficiencia
prop_and :: BooleanLambda -> BooleanLambda -> Bool
prop_and (BooleanLambda x) (BooleanLambda y)
    | x == falso || y == falso = f == falso
    | otherwise                = f == verdadero
    where f = (substitute puertaAND bindings <\ x) <\ y

prop_or :: BooleanLambda -> BooleanLambda -> Bool
prop_or (BooleanLambda x) (BooleanLambda y)
    | x == verdadero || y == verdadero = f == verdadero
    | otherwise                        = f == falso
    where f = (substitute puertaOR bindings <\ x) <\ y

prop_not :: BooleanLambda -> Bool
prop_not (BooleanLambda x) | x == verdadero = f == falso
                           | otherwise      = f == verdadero
    where f = substitute puertaNOT bindings <\ x

instance Arbitrary BooleanLambda where
    arbitrary = BooleanLambda <$> oneof [return falso, return verdadero]
