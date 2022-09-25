{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Chapter13.MonadParsers where

import Control.Applicative
import Data.Char

{- 
    A parser is a program that takes a string of characters as input, and produces
    some form of three that makes the syntatic structure of the string 
    explicit. 

    In Haskell, a parser can be naturally viewed as a function that takes a string
    and produces a tree. Since it could also not consume its entire string, it
    also returns any unconsumed part of the argument string, and since it may not
    always succeed, so the definition is further generalised to return a list of 
    results, an empty list denoting failure, and a singleton list returning success.

    Trees might be of different types and contain any kind of value, so it useful
    to abstract the specific type Tree and make it into a parameter:

    type Parser a = String -> [(a, String)]

    This is similar to the state transformer 

    State a = State -> [(a, State)]

    Where the state is manipulated as a string. The key difference is that a Parser
    can fail by returning a list of results.
-}

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) inp = p inp

item :: Parser Char
item = Parser (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)])

{-
    Parsers can be made into instances of the Functor, Applicative, and Monad 
    classes, in order to use the do notation to combine parsers in sequence.

    fmap applies a function to the result value of a parser if the parser succeeds, 
    and propagates the failure otherwise.
-}

instance Functor Parser where
    fmap g p = Parser (\inp -> case parse p inp of
        [] -> []
        [(v, out)] -> [(g v, out )])

{-
    <*> applies a parser that returns a function to a parser that returns 
    an argument to give a parser that returns the result of applying the 
    function to the argument, and only succeeds if all the components succeed
-}

instance Applicative Parser where
    pure v = Parser (\inp -> [(v, inp)])
    pg <*> px = Parser (\inp -> case parse pg inp of
        [] -> []
        [(g, out)] -> parse (fmap g px) out)

{- 
    For example, a parser that consumes three characters, discards the second, 
    and returns the first and third as a pair can now be defined in applicative 
    style:
-}

three :: Parser (Char,Char)
three = g <$> item <*> item <*> item
    where g x _ z = (x,z)

-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
instance Monad Parser where
    p >>= f = Parser (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

{-
    The parser p >>= f fails if the application of the parser p to the input 
    string inp fails, and otherwise applies the function f to the result 
    value v to give another parser f v, which is then applied to the output 
    string out that was produced by the first parser to give the final result. 
    Because Parser is a monadic type, the do notation can now be used to sequence 
    parsers and process their result values. For example, the parser three 
    can be defined in an alternative manner as follows:
-}

threeM :: Parser (Char,Char)
threeM = do
    x <- item
    _ <- item
    z <- item
    return (x,z)

{-
    Alternative is defined in Control.Applicative:

    class Applicative f => Alternative f where
        empty :: f a
        (<|>) :: f a -> f a -> f a
        many :: f a -> f [a]
        some :: f a -> f [a]

    For an Applicative to be an instance of Alternative, it must support empty and
    <|> primitives. Empty represents an alternative that has failed, and <|>
    is a choice operator for the type:

    instance Alternative Maybe where
        empty = Nothing
        Nothing <|> my = my
        (Just x) <|> _ = Just x

    The instance for the parser type is an extension of this idea, where empty is
    the parser that always fails regardless of the input string, and <|> is a choice
    operator that returns result the of the first parser if it succeeds on the input,
    and applies the second parser to the same input otherwise

    For example:

    parse empty "abc" => []
    parse (item <|> return ’d’) "abc" => [(’a’,"bc")]
    parse (empty <|> return ’d’) "abc" => [(’d’,"abc")]
-}

instance Alternative Parser where
    empty = Parser (const [])
    p <|> q = Parser (\inp -> case parse p inp of
        [] -> parse q inp
        [(v,out)] -> [(v,out)])

{-
    The parsers item, return v and empty, in combination with sequencing and choice,
    can be used to define other useful parsers. First of all, a parser sat p for 
    single characters that satisgy the predicate p:
-}

sat :: (Char -> Bool) -> Parser Char 
sat p = do
    x <- item 
    if p x then return x else empty 

{-
    Using sat and other predicates from the Data.Char library, other parsers can be
    defined (single digits, lower case letters, upper case letter, etc)
-}

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- using char, a parser for strings can be defined as well:

string :: String -> Parser String 
string [] = return []
string (x:xs) = do 
    _ <- char x
    _ <- string xs 
    return (x:xs) 

{-
    Many p and some p apply a parser as many times as possible until it fails,
    with the result values from each successful application of p being returned 
    in a list. The difference is that many permits 0 or more application, while
    some requires at least one. 

    many x = some x <|> pure []
    some x = pure (:) <*> x <*> many x

    These two functions allow the definition of parsers for identifiers (variables)
    comprising a lower case letter followed by zero or more alphanumeric chars,
    natural numbers composed of one or more digits, and spacing of zero of more
    space, tab, and newline characters:

-}

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x: xs)

nat :: Parser Int 
nat = do 
    xs <- some digit 
    return (read xs)

space :: Parser ()
space = do 
    _ <- many (sat isSpace)
    return ()

int :: Parser Int 
int = do 
    _ <- char '-'
    n <- nat
    return (-n)
    <|> nat 

{-
    Most real-life parser allow spacing to be used freely around the basic tokens
    in their input string. To handle such spacing, a new primitive must defined,
    one that ignores any space before an after applying a parser for a token:
-}

token :: Parser a -> Parser a
token p = do
    space 
    v <- p
    space 
    return v 

{-
    Using token, we can now define parsers that ignore spacing around identifiers, 
    natural numbers, integers and special symbols:
-}

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do 
    _ <- symbol "["
    n <- natural
    ns <- many (do 
            _ <- symbol "," 
            natural)
    _ <- symbol "]"
    return (n:ns)