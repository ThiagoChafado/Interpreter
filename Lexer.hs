module Lexer where

import Data.Char

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | If Expr Expr Expr
          | Lam String Ty Expr
          | App Expr Expr
          | Var String
          | MyLet String Expr Expr
          | MyList [Expr]          -- Adicionado para representar listas
          deriving Show

data Ty = TBool
        | TNum
        | TFun Ty Ty
        | TList Ty               -- Adicionado para o tipo lista
        deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenAdd
           | TokenAnd
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenLet
           | TokenMul
           | TokenSub
           | TokenLBracket        -- Token para '['
           | TokenRBracket        -- Token para ']'
           | TokenComma           -- Token para ','
           deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('[':cs) = TokenLBracket : lexer cs   -- Regra para '['
lexer (']':cs) = TokenRBracket : lexer cs   -- Regra para ']'
lexer (',':cs) = TokenComma : lexer cs       -- Regra para ','
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('*':cs) = TokenMul : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKw (c:cs)


lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of
                (num, rest) -> TokenNum (read num) : lexer rest

lexKw :: String -> [Token]
lexKw cs = case span isAlpha cs of
                ("true", rest)  -> TokenTrue : lexer rest
                ("false", rest) -> TokenFalse : lexer rest
                ("if", rest)    -> TokenIf : lexer rest
                ("then", rest)  -> TokenThen : lexer rest
                ("else", rest)  -> TokenElse : lexer rest
                _ -> error $ "Unknown keyword: "