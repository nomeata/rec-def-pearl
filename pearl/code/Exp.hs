module Exp where

type V    =  String
data Exp  =  Var V | Lam V Exp | App Exp Exp | Throw | Catch Exp
          |  Let V Exp Exp | LetRec [(V, Exp)] Exp
    deriving Show

someVal :: Exp
someVal = Lam "y" (Var "y")

example1 :: Exp
example1 = Let "x" Throw someVal

example2 :: Exp
example2 = Let "x" Throw (App (Var "x") someVal)

example3 :: Exp
example3 = LetRec [("x", Throw)] (App (Var "x") someVal)

example4 :: Exp
example4 = LetRec [("x", App (Var "x") Throw)] (App (Var "x") someVal)

example5 :: Exp
example5 = LetRec [("x", App (Var "x") someVal)] (App (Var "x") someVal)

example6 :: Exp
example6 = LetRec [("x", Var "x")] (Catch (Var "x"))

example7 :: Exp
example7 = LetRec
    [ ("x", App (Catch (Var "x")) someVal)
    ]
    (App (Var "x") someVal)
