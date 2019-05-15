module Lambda.Internals.LambdaParserTest where

import  Lambda

import           Lambda.Internals.LambdaTypeGenerators.LambdaTypeGenerator
import           Test.QuickCheck
import           Test.Hspec

-- | Parsear la expresión lambda que representa a @x@ debe ser igual al propio @x@
prop_idemparseo :: LambdaTerm -> Bool
prop_idemparseo x = x == read (toString x)

parserTest =
    describe "Internals.LambdaParser" $
    it "Parsea correctamente expresiones lambda arbitrarias" $
    property prop_idemparseo
