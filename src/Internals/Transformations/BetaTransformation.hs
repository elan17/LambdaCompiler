module Internals.Transformations.BetaTransformation where

import Internals.LambdaTypes
import Data.HashMap.Strict as H
import Data.Maybe

betaTransform :: LambdaTerm -> LambdaTerm
betaTransform t = helperBeta t H.empty
    where helperBeta (LambdaVariable x) m = fromMaybe (LambdaVariable x) (H.lookup x m)
          helperBeta x@(LambdaStub _) _ = x
          helperBeta (LambdaApplication (LambdaFunction (Parametros (x:xs)) t1) t2) m =
              case xs of
                  [] -> t1'
                  l -> LambdaFunction (Parametros l) t1'
                  where t1' = helperBeta t1 (H.insert x t2 m)

          --TODO: Edgecase -> ((x y) z)
          helperBeta (LambdaApplication t1 t2) m =
              case t1' of
                  (LambdaFunction _ x) -> helperBeta (LambdaApplication t1' t2') m
                  x -> LambdaApplication t1' t2'
            where t1' = helperBeta t1 m
                  t2' = helperBeta t2 m
          helperBeta (LambdaFunction (Parametros []) t) m = helperBeta t m
          helperBeta (LambdaFunction p t) m = LambdaFunction p $ helperBeta t m
