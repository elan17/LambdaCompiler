module Lambda.Internals.Instances.Equality where

import           Lambda.Internals.Transformations.AlphaTransformation
import           Lambda.Internals.Transformations.BetaTransformation
import           Lambda.Internals.LambdaTypes

instance Eq LambdaTerm where
    (==) t1 t2 = helper (alphaTransform $ betaTransform t1)
                        (alphaTransform $ betaTransform t2)
      where
        helper (LambdaStub     x) (LambdaStub     y) = x == y
        helper (LambdaVariable x) (LambdaVariable y) = x == y
        helper (LambdaApplication t1 t2) (LambdaApplication t3 t4) =
            helper t1 t3 && helper t2 t4
        helper (LambdaFunction (Parametros xs) t1) (LambdaFunction (Parametros ys) t2)
            = (xs == ys) && helper t1 t2
        helper _ _ = False
