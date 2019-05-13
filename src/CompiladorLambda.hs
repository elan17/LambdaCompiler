module CompiladorLambda
    ( LambdaTerm(..)
    , Parametros(..)
    , alphaTransform
    , betaTransform
    , substitute
    , (<\)
    , (\>)
    , applyFunction
    , toString
    , parseTerm
    )
where

import           Internals.Transformations.AlphaTransformation
import           Internals.Transformations.BetaTransformation
import           Internals.Transformations.SubstituteStubs
import           Internals.Instances.Equality
import           Internals.LambdaParser
import           Internals.LambdaTypes

import           Operations.Arithmetics.Integers
import           Operations.Logic
