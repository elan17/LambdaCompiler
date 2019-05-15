module Lambda.Operations.Logic.Internals.Definition where

import           Lambda.Internals.LambdaTypes
import           Lambda.Internals.LambdaParser
import           Lambda.Internals.Transformations.SubstituteStubs
import           Lambda.Internals.Transformations.BetaTransformation
import           Lambda.Internals.Instances.Equality

import           Data.HashMap.Strict           as H

-- | Valor de TRUE
verdadero = read "(λa b.a)" :: LambdaTerm
-- | Valor de FALSE
falso = read "(λa b.b)" :: LambdaTerm

puertaAND = read "(λx y.x y x)" :: LambdaTerm
puertaOR = read "(λx y.x x y)" :: LambdaTerm
puertaNOT = read "(λx.x FALSE TRUE)" :: LambdaTerm

-- | Mnemónicos de las expresiones del módulo
bindings = H.fromList
    [ ("TRUE" , verdadero)
    , ("FALSE", falso)
    , ("NOT"  , puertaNOT)
    , ("AND"  , puertaAND)
    , ("OR"   , puertaOR)
    ]

-- | Aplica la puerta and a los 2 operandos
(&&) :: LambdaTerm -> LambdaTerm -> LambdaTerm
(&&) x y = puertaAND <\ x <\ y

-- | Aplica la puerta not a los 2 operandos
(||) :: LambdaTerm -> LambdaTerm -> LambdaTerm
(||) x y = puertaOR <\ x <\ y

-- | Aplica la puerta not al operando
(!) :: LambdaTerm -> LambdaTerm
(!) x = puertaNOT <\ x

-- | Obtiene la expresión lambda asociada al valor booleano concreto
fromBoolean :: Bool -> LambdaTerm
fromBoolean True = verdadero
fromBoolean False = falso

-- | Obtiene el booleano que representa la expresión
toBoolean :: LambdaTerm -> Maybe Bool
toBoolean x | x == falso     = Just False
            | x == verdadero = Just True
            | otherwise      = Nothing
