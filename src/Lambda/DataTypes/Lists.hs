module Lambda.DataTypes.Lists
( nil
, cons
, headl
, taill
, dropl
, index
, Lambda.DataTypes.Lists.fromList
, bindings
) where

import Lambda.DataTypes.Tuples hiding (bindings)
import Lambda

import Data.HashMap.Strict as H

nil = read "FALSE" :: LambdaTerm
cons = read "PAIR" :: LambdaTerm
headl = read "FIRST" :: LambdaTerm
taill = read "SECOND" :: LambdaTerm
dropl = read "(λn l.n TAIL l)"
index = read "(λn l.HEAD (DROP n l))" :: LambdaTerm

fromList :: [LambdaTerm] -> LambdaTerm
fromList [] = nil
fromList (x:xs) = x \> cons <\ Lambda.DataTypes.Lists.fromList xs

bindings = H.fromList [ (":", cons)
                      , ("HEAD", headl)
                      , ("TAIL", taill)
                      , ("INDEX", index)
                      , ("DROP", dropl)] :: Bindings
