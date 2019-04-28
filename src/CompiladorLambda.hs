module CompiladorLambda
( LambdaTerm (..)
, Parametros (..)
, alphaTransform
, toString
, parseTerm
) where

import Internals.Transformations.AlphaTransformation
import Internals.LambdaParser
import Internals.LambdaTypes
