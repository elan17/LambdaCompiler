module Logic.Internals.Definition where

import Internals.LambdaTypes
import Internals.LambdaParser
import Internals.Transformations.SubstituteStubs

import Data.HashMap.Strict as H

verdadero = read "(λa b.a)" :: LambdaTerm
falso = read "(λa b.b)" :: LambdaTerm

puertaAND = read "(λx y.((x y) x))" :: LambdaTerm
puertaOR = read "(λx y.((x x) y))" :: LambdaTerm
puertaNOT = read "(λx.((x FALSE) TRUE))" :: LambdaTerm

bindings = H.fromList [ ("TRUE", verdadero)
                      , ("FALSE", falso)
                      , ("NOT", puertaNOT)
                      , ("AND", puertaAND)
                      , ("OR", puertaOR) ]
