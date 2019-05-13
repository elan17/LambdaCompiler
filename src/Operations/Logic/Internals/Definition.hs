module Operations.Logic.Internals.Definition where

import           Internals.LambdaTypes
import           Internals.LambdaParser
import           Internals.Transformations.SubstituteStubs
import           Internals.Transformations.BetaTransformation
import           Internals.Instances.Equality

import           Data.HashMap.Strict           as H

verdadero = read "(λa b.a)" :: LambdaTerm
falso = read "(λa b.b)" :: LambdaTerm

puertaAND = read "(λx y.x y x)" :: LambdaTerm
puertaOR = read "(λx y.x x y)" :: LambdaTerm
puertaNOT = read "(λx.x FALSE TRUE)" :: LambdaTerm

bindings = H.fromList
    [ ("TRUE" , verdadero)
    , ("FALSE", falso)
    , ("NOT"  , puertaNOT)
    , ("AND"  , puertaAND)
    , ("OR"   , puertaOR)
    ]

(&&) :: LambdaTerm -> LambdaTerm -> LambdaTerm
(&&) x y = puertaAND <\ x <\ y

(||) :: LambdaTerm -> LambdaTerm -> LambdaTerm
(||) x y = puertaOR <\ x <\ y

(!) :: LambdaTerm -> LambdaTerm
(!) x = puertaNOT <\ x

toBoolean :: LambdaTerm -> Maybe Bool
toBoolean x | x == falso     = Just False
            | x == verdadero = Just True
            | otherwise      = Nothing
