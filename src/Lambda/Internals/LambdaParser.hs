{-# LANGUAGE FlexibleInstances #-}
module Lambda.Internals.LambdaParser
( parseTerm
) where
import           Lambda.Internals.LambdaTypes
import           Text.Parsec

literalParser = do
    var <- many1 (oneOf "abcdefghijklmnñopqrstuvwxyz")
    notFollowedBy stubParser
    return var

-- | Parser para un stub
stubParser = LambdaStub <$> many1 (noneOf " .()λ")

-- | Parser para una variable
variableParser = LambdaVariable <$> literalParser

-- | Parser de los parametros de una función
paramsParser = do
    params <- sepBy literalParser (char ' ')
    return $ Parametros params

-- Parser de cualquier expresión lambda
termParser =
    try functionParser
        <|> try applicationParser
        <|> try variableParser
        <|> stubParser

-- | Parser de una aplicación lambda
applicationParser = do
    string "("
    returneo <- bodyParser
    string ")"
    return returneo

-- | Parser de una función lambda
functionParser = do
    char '('
    char 'λ'
    params <- paramsParser
    char '.'
    term <- bodyParser
    char ')'
    return $ LambdaFunction params term

bodyParser = do
    lista <- sepBy termParser $ char ' '
    return $ foldl1 LambdaApplication lista

-- | Parsea una expresión lambda dada
parseTerm :: String -> Either ParseError LambdaTerm
parseTerm = parse termParser "(unknown)"

instance Read LambdaTerm where
    readsPrec _ str = case parseTerm str of
        Right x -> [(x, "")]
        Left  x -> []
