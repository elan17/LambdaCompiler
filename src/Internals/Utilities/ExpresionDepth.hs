module Internals.Utilities.ExpresionDepth where

import           Internals.LambdaTypes

-- | Obtiene la profundidad de la expresión lambda
getDepth :: LambdaTerm -> Integer
getDepth t = getDepthHelper t 0
  where
    getDepthHelper (LambdaVariable _  ) n = n
    getDepthHelper (LambdaStub     _  ) n = n
    getDepthHelper (LambdaFunction _ t) n = getDepthHelper t (n + 1)
    getDepthHelper (LambdaApplication t1 t2) n =
        max (getDepthHelper t1 n) (getDepthHelper t2 n)
