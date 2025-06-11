module Interpreter where

import Lexer

isValue :: Expr -> Bool
isValue BTrue = True
isValue BFalse = True
isValue (Num _) = True
isValue (Lam _ _ _) = True
isValue (MyLet _ _ _) = True
isValue (MyList es) = all isValue es -- Uma lista é um valor se todos os elementos são valores
isValue _ = False


subst :: String -> Expr -> Expr -> Expr
subst v e BTrue = BTrue
subst v e BFalse = BFalse
subst v e (Num n) = Num n
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (Mul e1 e2) = Mul (subst v e e1) (subst v e e2)
subst v e (Sub e1 e2) = Sub (subst v e e1) (subst v e e2)
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Var x) = if x == v then e else Var x
subst v e (Lam x t b) = Lam x t (subst v e b)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (MyList es) = MyList (map (subst v e) es) -- Adicionado caso para MyList
subst v e (MyLet x e1 e2) = MyLet x (subst v e e1) (subst v e e2) -- Adicionado caso para MyLet


step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1 + n2)     -- S-Add
step (Add (Num n1) e2) = let e2' = step e2       -- S-Add2
                           in Add (Num n1) e2'
step (Add e1 e2) = Add (step e1) e2              -- S-Add1
step (Mul (Num n1) (Num n2)) = Num (n1 * n2)     -- S-Mul
step (Mul (Num n1) e2) = let e2' = step e2       -- S-Mul2
                           in Mul (Num n1) e2'
step (Mul e1 e2) = Mul (step e1) e2              -- S-Mul1
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)     -- S-Sub
step (Sub (Num n1) e2) = let e2' = step e2       -- S-Sub2
                           in Sub (Num n1) e2'
step (Sub e1 e2) = Sub (step e1) e2              -- S-Sub1
step (And BTrue e2) = e2
step (And BFalse e2) = BFalse
step (And e1 e2) = And (step e1) e2
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2
step (App e1@(Lam x t b) e2) | isValue e2 = subst x e2 b
                           | otherwise = App e1 (step e2)

step (MyLet x e1 e2)
  | isValue e1 = subst x e1 e2
  | otherwise  = MyLet x (step e1) e2

-- Regra de avaliação para listas
step (MyList es) = MyList (evalStep es)
  where evalStep [] = []
        evalStep (h:t) | isValue h = h : evalStep t
                       | otherwise = step h : t


eval :: Expr -> Expr
eval e | isValue e = e
       | otherwise = eval (step e)