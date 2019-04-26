{-# LANGUAGE FlexibleInstances #-}
module LambdaParser where

import LambdaTypes
import Text.Parsec

literalParser = many1 $ noneOf " .()λ"

variableParser = LambdaVariable <$> literalParser

paramsParser = do params <- sepBy literalParser (char ' ')
                  return $ Parametros params

termParser =  try functionParser
          <|> try applicationParser
          <|> variableParser

applicationParser = do string "("
                       term1 <- termParser
                       char ' '
                       term2 <- termParser
                       string ")"
                       return $ LambdaApplication term1 term2

-- TODO Reportar bug en Parsec: string "(λ" /= do char '('; char 'λ'
functionParser = do char '('
                    char 'λ'
                    params <- paramsParser
                    char '.'
                    term <- termParser
                    char ')'
                    return $ LambdaFunction params term

parseTerm :: String -> Either ParseError (LambdaTerm String)
parseTerm = parse termParser "(unknown)"


instance Read (LambdaTerm String) where
    readsPrec _ str = case parseTerm str of
                        Right x -> [(x, "")]
                        Left x -> []
