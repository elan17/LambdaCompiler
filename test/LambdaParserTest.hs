module LambdaParserTest where

import LambdaParser
import LambdaTypes

prop_idemparseo :: LambdaTerm String -> Bool
prop_idemparseo x = x == read (show x)
