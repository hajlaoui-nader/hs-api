module Lib
    ( someFunc
    )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myConcat :: [[a]] -> [a]
myConcat []       = []
myConcat (x : xs) = x ++ myConcat xs

def :: String
def = "hey"


data Parser x = Parser(String -> Maybe (String, x))
data Unit = Unit deriving Show

unit :: Parser Unit
unit = Parser (\s -> Just (s, Unit))


parseBool :: Parser Bool
parseBool = Parser
    (\s -> case s of
        't' : z -> Just (z, True)
        'f' : z -> Just (z, False)
        _       -> Nothing
    )

fail :: Parser x
fail = Parser (const Nothing)

runParser :: Parser x -> String -> Maybe (String, x)
runParser (Parser f) = f

composeParsers :: Parser a -> Parser b -> Parser (a, b)
composeParsers pa pb = Parser
    (\s0 -> case runParser pa s0 of
        Nothing      -> Nothing
        Just (s1, a) -> case runParser pb s1 of
            Nothing      -> Nothing
            Just (s2, b) -> Just (s2, (a, b))
    )

instance Functor Parser where
    fmap f pa = Parser
        (\s0 -> case runParser pa s0 of
            Nothing      -> Nothing
            Just (s1, v) -> Just (s1, f v)
        )
