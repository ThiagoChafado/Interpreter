module TypeChecker where

import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
typeof ctx (Num _) = Just TNum
typeof ctx BFalse = Just TBool
typeof ctx BTrue = Just TBool
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       _                      -> Nothing
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing
typeof ctx (If e1 e2 e3) =
    case typeof ctx e1 of
      Just TBool -> case (typeof ctx e2, typeof ctx e3) of
                      (Just t1, Just t2) | t1 == t2  -> Just t1
                                         | otherwise -> Nothing
                      _ -> Nothing
      _ -> Nothing

typeof ctx (Mul e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       _                      -> Nothing
typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       _                      -> Nothing

typeof ctx (Var v) = lookup v ctx
typeof ctx (Lam x t b) = case typeof ((x, t) : ctx) b of
                      Just t2 -> Just (TFun t t2)
                      _       -> Nothing

typeof ctx (App e1 e2) = case typeof ctx e1 of
      Just (TFun t1 t2) -> case typeof ctx e2 of
            Just t3 | t1 == t3 -> Just t2
                    | otherwise -> Nothing
            _ -> Nothing
      _ -> Nothing

-- Caso para MyList
typeof ctx (MyList []) = Nothing -- Impossível determinar o tipo de uma lista vazia isoladamente
typeof ctx (MyList (e:es)) =
    case typeof ctx e of
        Nothing -> Nothing
        Just t  -> if all (== Just t) (map (typeof ctx) es)
                   then Just (TList t)
                   else Nothing

typecheck :: Expr -> Expr
typecheck e = case typeof [] e of
    Just _ -> e
    Nothing -> error "Type error"