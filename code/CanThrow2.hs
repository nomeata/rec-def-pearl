import qualified Data.Map as M
import Data.Recursive.Bool (RBool)
import qualified Data.Recursive.Bool as RB

import Exp

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

