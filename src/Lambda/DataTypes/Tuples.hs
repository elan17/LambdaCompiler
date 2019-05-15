module Lambda.DataTypes.Tuples where

import Lambda
import Data.HashMap.Strict as H

pair = read "(λa b f.f a b)" :: LambdaTerm
first = read "(λp.p TRUE)" :: LambdaTerm
second = read "(λp.p FALSE)" :: LambdaTerm

bindings = H.fromList
    [ ("PAIR" , pair)
    , ("FIRST", first)
    , ("SECOND", second)
    ]
