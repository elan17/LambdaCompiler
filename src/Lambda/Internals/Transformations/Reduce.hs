module Lambda.Internals.Transformations.Reduce where

import           Lambda.Internals.Transformations.BetaTransformation
import           Lambda.Internals.Transformations.AlphaTransformation
import           Lambda.Internals.Transformations.SubstituteStubs
import           Lambda.Internals.LambdaTypes
import           Lambda.Internals.Instances.Equality

reduce :: Bindings -> LambdaTerm -> LambdaTerm
reduce b t = helper b t (LambdaVariable "x")
  where
    helper b t1 t2 =
        if t1 == t2 then t1 else helper b (substitute (betaTransform (alphaTransform t1)) b) t1
