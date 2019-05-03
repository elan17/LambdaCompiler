module Internals.Transformations.BetaTransformationTest where

import CompiladorLambda
import Internals.LambdaTypeGenerators.HighCollisionGenerator

prop_Monoid :: HighCollisionLambdaType -> HighCollisionLambdaType -> Bool
prop_Monoid (HighCollisionLambdaType original) (HighCollisionLambdaType ap) =
    betaTransform ( LambdaApplication(betaTransform original) ap) ==
        betaTransform (LambdaApplication original ap)
