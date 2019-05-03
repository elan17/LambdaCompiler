module Internals.Transformations.BetaTransformationTest where

import CompiladorLambda
import Internals.LambdaTypeGenerators.HighCollisionGenerator

prop_Composition :: HighCollisionLambdaType -> HighCollisionLambdaType -> Bool
prop_Composition (HighCollisionLambdaType original) (HighCollisionLambdaType ap) =
    betaTransform original <\ ap == original <\ ap
