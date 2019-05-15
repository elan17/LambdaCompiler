module Lambda.Internals.Transformations.AlphaTransformationTest where

import  Lambda
import Data.HashSet as H
import Data.HashMap.Strict as M
import Lambda.Internals.LambdaTypeGenerators.LambdaTypeGenerator
import Lambda.Internals.LambdaTypeGenerators.HighCollisionGenerator

import           Test.QuickCheck
import           Test.Hspec

prop_mismaEstructura :: LambdaTerm -> Bool
prop_mismaEstructura original = helper original nueva
    where nueva = alphaTransform original
          helper (LambdaStub _) (LambdaStub _) = True
          helper (LambdaVariable _) (LambdaVariable _) = True
          helper (LambdaFunction _ t1) (LambdaFunction _ t2) = helper t1 t2
          helper (LambdaApplication t1 t2) (LambdaApplication t3 t4) =
              helper t1 t3 && helper t2 t4
          helper _ _ = False

prop_sinParametrosRepetidos :: HighCollisionLambdaType -> Bool
prop_sinParametrosRepetidos (HighCollisionLambdaType original) = helper nueva H.empty
    where nueva = alphaTransform original
          helper (LambdaFunction (Parametros l) x) set =
              case H.size (H.intersection set (H.fromList l)) of
                  0 -> helper x (set `H.union` H.fromList l)
                  _ -> False
          helper (LambdaApplication x y) set = helper x set && helper y set
          helper _ _ = True

prop_unboundIguales :: LambdaTerm -> Bool
prop_unboundIguales original =
    fst (helper original nueva M.empty M.empty)
    where nueva = alphaTransform original
          helper (LambdaVariable x) (LambdaVariable y) bound unbound =
              case M.lookup x bound of
                  Just _ -> (True, unbound)
                  Nothing -> case M.lookup x unbound of
                                Nothing -> (True, M.insert x y unbound)
                                Just z -> (z == y, unbound)
          helper (LambdaStub x) (LambdaStub y) bound unbound =
              (x == y, unbound)
          helper (LambdaFunction (Parametros xs) t1) (LambdaFunction (Parametros ys) t2) bound unbound =
              helper t1 t2 (M.union (M.fromList (zip xs ys)) bound) unbound
          helper (LambdaApplication t1 t2) (LambdaApplication t3 t4) bound unbound =
              (resul1 && resul2, M.union unbound1 unbound2)
              where (resul1, unbound1) = helper t1 t3 bound unbound
                    (resul2, unbound2) = helper t2 t4 bound unbound1

alphaTest =
    describe "Internals.Transformations.AlphaTransformation.alphaTransform" $ do
    it "Mantiene la estructura de la expresion lambda" $
        property prop_mismaEstructura
    it "No se repiten nombres de variables en los parámetros" $
        property prop_sinParametrosRepetidos
    it "Las variables unbound se mantienen con los mismos nombres" $
        property prop_unboundIguales
