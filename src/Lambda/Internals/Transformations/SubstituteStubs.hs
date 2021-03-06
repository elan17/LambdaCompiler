module Lambda.Internals.Transformations.SubstituteStubs where

import           Lambda.Internals.LambdaTypes
import           Data.HashMap.Strict           as M
import           Data.Maybe

type Bindings = HashMap String LambdaTerm

-- | Sustituye los stubs de la expresión a partir de los bindings
substitute :: LambdaTerm -> Bindings -> LambdaTerm
substitute x@(LambdaStub     v) b = fromMaybe x (M.lookup v b)
substitute x@(LambdaVariable _) _ = x
substitute (LambdaApplication t1 t2) b =
    LambdaApplication (substitute t1 b) (substitute t2 b)
substitute (LambdaFunction p t) b = LambdaFunction p (substitute t b)
