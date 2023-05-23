import qualified Data.Map as M
import Data.Recursive.Bool (RBool)
import qualified Data.Recursive.Bool as RB

type V    =  String
data Exp  =  Var V | Lam V Exp | App Exp Exp | Throw | Catch Exp
          |  Let V Exp Exp | LetRec [(V, Exp)] Exp

canThrow :: Exp -> Bool
canThrow e = go M.empty e
  where
    go :: M.Map V Bool -> Exp -> Bool
    go env (Var v)        = env M.! v
    go env Throw          = True
    go env (Catch e)      = False
    go env (Lam v e)      = go (M.insert v False env) e
    go env (App e1 e2)    = go env e1 || go env e2
    go env (Let v e1 e2)  = go env' e2
      where
        env_bind  = M.singleton v (go env e1)
        env'      = M.union env_bind env
    go env (LetRec binds e) = go env' e
      where
        env_bind  = M.fromList [ (v, go env' e) | (v,e) <- binds ]
        env'      = M.union env_bind env

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
example6 = LetRec [("x", Var "x")] (App (Var "x") someVal)

canThrow2 :: Exp -> Bool
canThrow2 e = RB.get (go M.empty e)
  where
    go :: M.Map V RBool -> Exp -> RBool
    go env (Var v)          = env M.! v
    go env Throw            = RB.true
    go env (Catch e)        = RB.false
    go env (Lam v e)        = go (M.insert v RB.false env) e
    go env (App e1 e2)      = go env e1 RB.|| go env e2
    go env (Let v e1 e2)    = go env' e2
      where
        env_bind  = M.singleton v (go env e1)
        env'      = M.union env_bind env
    go env (LetRec binds e) = go env' e
      where
        env_bind  = M.fromList [ (v, RB.id (go env' e)) | (v,e) <- binds ]
        env'      = M.union env_bind env

