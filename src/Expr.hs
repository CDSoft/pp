{- PP

Copyright (C) 2015-2021 Christophe Delord

http://cdelord.fr/pp

This file is part of PP.

PP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

PP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with PP.  If not, see <http://www.gnu.org/licenses/>.
-}

module Expr ( eval
            )
where

import Data.Bool
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)

import ErrorMessages

data V = Z Integer | S String

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser javaStyle

expr :: ParsecT String u Identity V
expr = buildExpressionParser table term <?> "expression"

term :: ParsecT String u Identity V
term = parens lexer expr
     <|> braces lexer expr
     <|> brackets lexer expr
     <|> (Z <$> integer lexer)
     <|> (S <$> stringLiteral lexer)
     <|> (S <$> identifier lexer)
     <?> "term"

table :: [[Operator String u Identity V]]
table = [ [prefix "-" (op1 negate), prefix "+" (op1 id)]
        , [binary "*" (op2 (*)) AssocLeft, binary "/" (op2 div) AssocLeft]
        , [binary "+" (op2 (+)) AssocLeft, binary "-" (op2 (-)) AssocLeft]
        , [prefix "!" (bool1 not), prefix "not" (bool1 not)]
        , [binary "&&" (bool2 (&&)) AssocLeft, binary "and" (bool2 (&&)) AssocLeft]
        , [binary "||" (bool2 (||)) AssocLeft, binary "or" (bool2 (||)) AssocLeft]
        , [binary "xor" (bool2 (/=)) AssocLeft]
        , [ binary "==" (rel (==) (==)) AssocLeft
          , binary "/=" (rel (/=) (/=)) AssocLeft
          , binary "!=" (rel (/=) (/=)) AssocLeft
          , binary "<=" (rel (<=) (<=)) AssocLeft
          , binary "<" (rel (<) (<)) AssocLeft
          , binary ">=" (rel (>=) (>=)) AssocLeft
          , binary ">" (rel (>) (>)) AssocLeft
          ]
        ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary name fun = Infix (do { reservedOp lexer name; return fun })
prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fun = Prefix (do { reservedOp lexer name; return fun })

op1 :: (Integer -> Integer) -> V -> V
op1 op (Z x) = Z $ op x
op1 op (S s) = Z $ op $ atoi s

op2 :: (Integer -> Integer -> Integer) -> V -> V -> V
op2 op (Z x) (Z y) = Z $ op x y
op2 op (Z x) (S y) = Z $ op x (atoi y)
op2 op (S x) (Z y) = Z $ op (atoi x) y
op2 op (S x) (S y) = Z $ op (atoi x) (atoi y)

b :: Bool -> V
b = Z . bool 0 1

bool1 :: (Bool -> Bool) -> V -> V
bool1 op (Z x) = b $ op (x /= 0)
bool1 op (S s) = b $ op (s /= "")

bool2 :: (Bool -> Bool -> Bool) -> V -> V -> V
bool2 op (Z x) (Z y) = b $ op (x /= 0) (y /= 0)
bool2 op (Z x) (S y) = b $ op (x /= 0) (y /= "")
bool2 op (S x) (Z y) = b $ op (x /= "") (y /= 0)
bool2 op (S x) (S y) = b $ op (x /= "") (y /= "")

rel :: (Integer -> Integer -> Bool) -> (String -> String -> Bool) -> V -> V -> V
rel ri _rs (Z x) (Z y) = b $ ri x y
rel _ri rs (Z x) (S y) = b $ rs (show x) y
rel _ri rs (S x) (Z y) = b $ rs x (show y)
rel _ri rs (S x) (S y) = b $ rs x y

atoi :: String -> Integer
atoi s = case reads s of
            [(i, "")] -> i
            _ -> 0

contents :: Parser a -> Parser a
contents p = do
    r <- p
    eof
    return r

eval :: SourceName -> String -> (String, Bool)
eval source expression = case parse (contents expr) source expression of
    Right (Z n) -> (show n, n /= 0)
    Right (S s) -> (s, not (null s))
    Left _ -> exprError expression
