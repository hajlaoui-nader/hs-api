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


data Parser x = Parser(String -> Either (Integer,String) (Integer, String, x))
data Unit = Unit deriving Show

unitP :: Parser Unit
unitP = Parser (\s -> Right (0, s, Unit))


parseBool :: Parser Bool
parseBool = parse1
    (\s -> case s of
        't' -> Right True
        'f' -> Right False
        _   -> Left "expected 't' or 'f'"
    )

-- fail :: Parser x
-- fail = Parser (const Nothing)

runParser :: Parser x -> String -> Either (Integer, String) (Integer, String, x)
runParser (Parser f) = f

composeParsers :: Parser a -> Parser b -> Parser (a, b)
composeParsers pa pb = Parser
    (\s0 -> case runParser pa s0 of
        Left  e           -> Left e
        Right (n0, s1, a) -> case runParser pb s1 of
            Left  (n1, msg)   -> Left (n0 + n1, msg)
            Right (n1, s2, b) -> Right (n0 + n1, s2, (a, b))
    )

instance Functor Parser where
    fmap f pa = Parser
        (\s0 -> case runParser pa s0 of
            Left  e           -> Left e
            Right (n1, s1, v) -> Right (n1, s1, f v)
        )

class Functor f => Monoidal f where
    unit :: f Unit
    pair :: f a -> f b -> f (a,b)

instance Monoidal Parser where
    unit = unitP
    pair = composeParsers

boolFromChar :: Char -> Either String Bool
boolFromChar c = case c of
    't' -> Right True
    'f' -> Right False
    _   -> Left "Expected 't' or 'f'"


parse1 :: (Char -> Either String x) -> Parser x
parse1 f = Parser
    (\s0 -> case s0 of
        []     -> Left (0, "Not enough input")
        c : s1 -> case f c of
            Left  e -> Left (0, e)
            Right x -> Right (1, s1, x)
    )

expect1 :: Char -> Parser Unit
expect1 c = parse1
    (\s0 -> case c == s0 of
        True  -> Right Unit
        False -> Left ("Expected " <> show c <> " got " <> show s0)
    )

expect :: String -> Parser Unit
expect = foldr (\x z -> fmap (const Unit) (pair (expect1 x) z)) unit

seq_ :: Monoidal f => [f x] -> f [x]
seq_ f = case f of
    []        -> Lib.pure []
    fx : rest -> fmap (\(x, xs) -> x : xs) (pair fx (seq_ rest))

pure :: Monoidal f => a -> f a
pure x = fmap (const x) unit

data Void

class Functor f => Monoidalt f where
    alt :: f a -> f b -> f (Either a b)
    voidd :: f Void

instance Monoidalt Parser where
    alt pa pb = Parser
        (\s0 -> case runParser pa s0 of
            Right (n, s1, a) -> Right (n, s1, Left a)
            Left  _          -> case runParser pb s0 of
                Left  x           -> Left x
                Right (n2, s2, b) -> Right (n2, s2, Right b)
        )
    voidd = Parser (const (Left (0, "void")))

instance Monoidal [] where
    unit = [Unit]
    pair []       _        = []
    pair _        []       = []
    pair (x : xs) (y : ys) = (x, y) : (pair xs ys)
