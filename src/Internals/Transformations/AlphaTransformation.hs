module Internals.Transformations.AlphaTransformation where

import Internals.LambdaTypes
import Data.Char
import Data.HashMap.Strict as M
import Data.Hashable

stringsGenerator :: [String]
stringsGenerator = helper "a"
    where helper item = item : helper (nextString item)

          nextString [] = ['a']
          nextString l@('z':xs) = 'a':nextString xs
          nextString l@(x:xs) = nextChar x : xs

          nextChar 'z' = 'a'
          nextChar c = chr (ord c + 1)

-- | @alphaTransform t@ realiza una transformación alpha a la expresión lambda
--   de tal manera que no haya colisión de nombres.
alphaTransform :: LambdaTerm -> LambdaTerm
alphaTransform t = (\(x, _, _) -> x) (helperAlpha t M.empty M.empty stringsGenerator)
    where helperAlpha stub@(LambdaStub _) _ unbound l = (stub, unbound, l)


          helperAlpha (LambdaVariable x) mapa unbound l@(a:as) =
                case M.lookup x mapa of
                    Just y -> (LambdaVariable y, unbound, l) -- Bound variable
                    Nothing -> case M.lookup x unbound of -- Unbound variable
                                    Just z -> (LambdaVariable z, unbound, l)
                                    Nothing -> (LambdaVariable a, M.insert x a unbound, as)


          helperAlpha (LambdaApplication x y) mapa unbound l =
                ( LambdaApplication resul1 resul2, newUnbound2, newList2)
                where (resul1, newUnbound1, newList1) =
                          helperAlpha x mapa unbound l
                      (resul2, newUnbound2, newList2) =
                          helperAlpha y mapa newUnbound1 newList1


          helperAlpha (LambdaFunction (Parametros xs) term) mapa unbound l =
                ( LambdaFunction (Parametros valores) resul, newUnbound, newList)
                where (resul, newUnbound, newList) =
                            helperAlpha term actualizaMapa unbound resto
                      (valores, resto) = splitAt (length xs) l
                      actualizaMapa = let nuevo = M.fromList $ zip xs valores
                                      in M.union nuevo mapa
