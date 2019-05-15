module Lambda.Internals.Transformations.BetaTransformationTest where

import Lambda
import Lambda.Internals.LambdaTypeGenerators.HighCollisionGenerator

import           Test.QuickCheck
import           Test.Hspec

prop_Composition :: HighCollisionLambdaType -> HighCollisionLambdaType -> Bool
prop_Composition (HighCollisionLambdaType original) (HighCollisionLambdaType ap) =
    betaTransform original <\ ap == original <\ ap

betaTest =
    describe "Internals.Trasformations.BetaTransformation.betaTransform" $
    it "Dado f(x) = betaTransform x, se cumple que f((t1 t2)) == f((f(t1) t2))" $
        property prop_Composition
