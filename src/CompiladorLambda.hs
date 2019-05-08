module CompiladorLambda
( LambdaTerm (..)
, Parametros (..)
, alphaTransform
, betaTransform
, (<\)
, (\>)
, applyFunction
, toString
, parseTerm
) where

import Internals.Transformations.AlphaTransformation
import Internals.Transformations.BetaTransformation
import Internals.LambdaParser
import Internals.LambdaTypes
import Arithmetics.Integers
import Internals.Instances.Equality
