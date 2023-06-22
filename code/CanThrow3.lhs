
\iffalse

> module CanThrow3 where
>
> import qualified Data.Map as M
> import Data.Recursive.Bool (RBool)
> import qualified Data.Recursive.Bool as RB
>
> import Exp

\fi

> removeCatch :: Exp -> Exp
> removeCatch e = snd (go M.empty e)
>   where
>     go :: M.Map V RBool -> Exp -> (RBool, Exp)
>     go env (Var v)           = (env M.! v, Var v)
>     go env Throw             = (RB.true, Throw)
>
>     go env (Catch e)         = (RB.false, new_e)
>       where
>         (can_throw, e')     = go env e
>         new_e  | RB.get can_throw   = Catch e'
>                | otherwise          = e'
>
>     go env (Lam v e)         = (can_throw, Lam v e')
>       where
>         env'                = M.insert v RB.false env
>         (can_throw, e')     = go env' e
>
>     go env (App e1 e2)       = (can_throw1 RB.|| can_throw2, App e1' e2')
>       where
>         (can_throw1, e1')   = go env e1
>         (can_throw2, e2')   = go env e2
>
>     go env (Let v e1 e2)     = (can_throw2, Let v e1' e2')
>       where
>         (can_throw1, e1')   = go env e1
>         env_bind            = M.singleton v can_throw1
>         env'                = M.union env_bind env
>         (can_throw2, e2')   = go env' e2
>
>     go env (LetRec binds e)  = (can_throw, LetRec binds' e')
>       where
>         (env_bind, binds')  = unzip (map (goBind env') binds)
>         env'                = M.union (M.fromList env_bind) env
>         (can_throw, e')     = go env' e
>
>     goBind :: M.Map V RBool -> (V,Exp) -> ((V,RBool), (V,Exp))
>     goBind env (v,e) = ((v, RB.id can_throw), (v, e'))
>       where
>         (can_throw, e')     = go env e
