module Lambda.DefaultBindings where

import Lambda

import Lambda.Operations.Logic as Logic
import Lambda.Operations.Arithmetics.Integers as I

import Lambda.DataTypes.Lists as Lists
import Lambda.DataTypes.Tuples as T

import Data.HashMap.Strict as H

defaultBindings = H.unions
                    [ Logic.bindings
                    , Lists.bindings
                    , I.bindings
                    , T.bindings ]

reduceDefault = reduce defaultBindings
