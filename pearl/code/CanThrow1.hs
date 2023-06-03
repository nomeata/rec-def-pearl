import qualified Data.Map as M
import Data.Recursive.Bool (RBool)
import qualified Data.Recursive.Bool as RB

import Exp

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
