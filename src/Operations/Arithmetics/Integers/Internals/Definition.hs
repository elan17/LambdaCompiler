module Operations.Arithmetics.Integers.Internals.Definition where

import           Internals.LambdaTypes
import           Internals.LambdaParser
import           Internals.Transformations.BetaTransformation
import           Internals.Utilities.ExpresionDepth
import           Internals.Instances.Equality

import           Data.HashMap.Strict           as H

zero = read "(λf z.z)" :: LambdaTerm
succesor = read "(λn f z.f (n f z))" :: LambdaTerm
plus = read "(λm n f z.m f (n f z))" :: LambdaTerm
mul = read "(λm n f z.m (n f) z)" :: LambdaTerm

instance Num LambdaTerm where
    fromInteger 0 = zero
    fromInteger x = succesor <\ (fromInteger (x - 1) :: LambdaTerm)

    (+) t1 t2 = (plus <\ t1) <\ t2
    (*) t1 t2 = (mul <\ t1) <\ t2

toInteger :: LambdaTerm -> Maybe Integer
toInteger x = if fromInteger candidato == x then Just candidato else Nothing
    where candidato = getDepth x

bindings = H.fromList [("+", plus), ("*", mul)]
