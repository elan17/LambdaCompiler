module Lambda.Operations.Arithmetics.Integers.Internals.Definition where

import           Lambda.Internals.LambdaTypes
import           Lambda.Internals.LambdaParser
import           Lambda.Internals.Transformations.BetaTransformation
import           Lambda.Internals.Utilities.ExpresionDepth
import           Lambda.Internals.Instances.Equality

import           Data.HashMap.Strict           as H

-- | Número 0
zero = read "(λf z.z)" :: LambdaTerm
-- | Función sucesora
succesor = read "(λn f z.f (n f z))" :: LambdaTerm
-- | Función suma
plus = read "(λm n f z.m f (n f z))" :: LambdaTerm
-- | Función multiplicación
mul = read "(λm n f z.m (n f) z)" :: LambdaTerm

instance Num LambdaTerm where
    fromInteger 0 = zero
    fromInteger x = succesor <\ (fromInteger (x - 1) :: LambdaTerm)

    (+) t1 t2 = (plus <\ t1) <\ t2
    (*) t1 t2 = (mul <\ t1) <\ t2

-- | Función que obtiene el entero que representa una expresión lambda
toInteger :: LambdaTerm -> Maybe Integer
toInteger x = if fromInteger candidato == x then Just candidato else Nothing
    where candidato = getDepth x - 1

-- | Mnemónicos de las funciones
bindings = H.fromList
                [("+", plus)
                , ("*", mul)]
