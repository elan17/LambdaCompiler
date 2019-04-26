{-# LANGUAGE FlexibleInstances #-}

module LambdaTypes
( LambdaTerm(..)
, Parametros(Parametros)
) where

newtype Parametros a = Parametros [a]
    deriving (Eq)

data LambdaTerm a = LambdaVariable a
                  | LambdaFunction { variables :: Parametros a
                                   , cuerpo :: LambdaTerm a}
                  | LambdaApplication (LambdaTerm a) (LambdaTerm a)
                  deriving (Eq)

{-
    Métodos para convertir la expresión lambda a un string
-}

instance Show (Parametros String) where
    show (Parametros []) = ""
    show (Parametros [x]) = x
    show (Parametros (x:xs)) = x ++ " " ++ show (Parametros xs)

instance Show (Parametros Char) where
    show (Parametros []) = ""
    show (Parametros [x]) = [x]
    show (Parametros (x:xs)) = x : ' ' : show (Parametros xs)

instance {-# OVERLAPS #-} (Show a) => Show (Parametros a) where
    show (Parametros []) = ""
    show (Parametros [x]) = show x
    show (Parametros (x:xs)) = show x ++ " " ++ show (Parametros xs)


instance Show (LambdaTerm Char) where
    show (LambdaVariable x) = [x]
    show (LambdaApplication x y) = "("++ show x ++ " " ++ show y++")"
    show (LambdaFunction vars c) = "(λ"++show vars++"."++show c++")"

instance Show (LambdaTerm String) where
    show (LambdaVariable x) = x
    show (LambdaApplication x y) = "("++ show x ++ " " ++ show y++")"
    show (LambdaFunction vars c) = "(λ"++show vars++"."++show c++")"

instance {-# OVERLAPS #-} (Show a) => Show (LambdaTerm a) where
    show (LambdaVariable x) = show x
    show (LambdaApplication x y) = "("++ show x ++ " " ++ show y++")"
    show (LambdaFunction vars c) = "(λ"++show vars++"."++show c++")"
