module CompiladorLambda
( LambdaTerm (..)
, Parametros (..)
, alphaTransform
, betaTransform
, toString
, parseTerm
) where

import Internals.Transformations.AlphaTransformation
import Internals.Transformations.BetaTransformation
import Internals.LambdaParser
import Internals.LambdaTypes
