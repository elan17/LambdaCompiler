module Internals.Transformations.BetaTransformation where

import Internals.LambdaTypes
import Data.HashMap.Strict as H
import Data.Maybe

-- | Aplica el término de la derecha al de la izquierda
(<\) = applyFunction
-- | Aplica el término de la izquierda al de la derecha
(\>) = flip applyFunction

-- | @applyFunction t1 t2@ aplica la expresión t2 a la expresión t1, realizando
-- una transformación beta
applyFunction :: LambdaTerm -> LambdaTerm -> LambdaTerm
applyFunction t1 t2 = betaTransform (LambdaApplication t1 t2)

-- | @betaTransform t@ aplica una tranformación beta a la expresión reduciendola
-- lo máximo posible
betaTransform :: LambdaTerm -> LambdaTerm
betaTransform t = helperBeta t H.empty
    where helperBeta (LambdaVariable x) m = fromMaybe (LambdaVariable x) (H.lookup x m)
          helperBeta x@(LambdaStub _) _ = x
          helperBeta (LambdaApplication (LambdaFunction (Parametros (x:xs)) t1) t2) m =
              case xs of
                  [] -> t1'
                  l -> LambdaFunction (Parametros l) t1'
                  where t1' = helperBeta t1 (H.insert x t2' m)
                        t2' = helperBeta t2 m
          helperBeta (LambdaApplication t1 t2) m =
              case t1' of
                  (LambdaFunction _ _) -> helperBeta (LambdaApplication t1' t2') m
                  _ -> LambdaApplication t1' t2'
            where t1' = helperBeta t1 m
                  t2' = helperBeta t2 m
          helperBeta (LambdaFunction (Parametros []) t) m = helperBeta t m
          helperBeta (LambdaFunction p t) m = LambdaFunction p $ helperBeta t m
