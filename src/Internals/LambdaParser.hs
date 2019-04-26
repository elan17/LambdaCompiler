{-# LANGUAGE FlexibleInstances #-}
module Internals.LambdaParser where

import Internals.LambdaTypes
import Text.Parsec

literalParser = many1 $ noneOf " .()λ"

-- | Parser para una variable
variableParser = LambdaVariable <$> literalParser

-- | Parser de los parametros de una función
paramsParser = do params <- sepBy literalParser (char ' ')
                  return $ Parametros params

-- Parser de cualquier expresión lambda
termParser =  try functionParser
          <|> try applicationParser
          <|> variableParser

-- | Parser de una aplicación lambda
applicationParser = do string "("
                       term1 <- termParser
                       char ' '
                       term2 <- termParser
                       string ")"
                       return $ LambdaApplication term1 term2

-- TODO Reportar bug en Parsec: string "(λ" /= do char '('; char 'λ'
-- | Parser de una función lambda
functionParser = do char '('
                    char 'λ'
                    params <- paramsParser
                    char '.'
                    term <- termParser
                    char ')'
                    return $ LambdaFunction params term

-- | Parsea una expresión lambda dada
parseTerm :: String -> Either ParseError (LambdaTerm String)
parseTerm = parse termParser "(unknown)"

instance Read (LambdaTerm String) where
    readsPrec _ str = case parseTerm str of
                           Right x -> [(x, "")]
                           Left x -> []