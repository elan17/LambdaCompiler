module Internals.LambdaParserTest where

import CompiladorLambda

-- | Parsear la expresión lambda que representa a @x@ debe ser igual al propio @x@
prop_idemparseo :: LambdaTerm String -> Bool
prop_idemparseo x = x == read (show x)
