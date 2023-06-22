{-# LANGUAGE TypeOperators #-}
import qualified Data.Map as M
import qualified Data.Set as S

import Hatafun

type V = String
type Env = M.Map V Bool
data Exp  = Var V | Lam V Exp | App Exp Exp | Throw | Catch Exp
          | Let V Exp Exp | LetRec (M.Map V Exp) Exp

canThrow :: Exp -> Bool
canThrow e = eval (go `mapp` lift M.empty `app` lift e)

go :: Defn (M.Map V Bool -+> (Exp -> Bool))
go = mlam $ \env -> lam $ \x -> case unlift x of
   Var v -> when  (mapMember `app` lift v `mapp` env)
                  (env ! lift v)
   Throw -> tt
   Catch e -> ff
   Lam v e -> go `mapp` lub (lift $ M.singleton v False) env `app` lift e
   App e1 e2 -> (go `mapp` env `app` lift e1) `lub` (go `mapp` env `app` lift e2)
   Let vUnsafe e1Unsafe e2Unsafe -> go `mapp` env' `app` e2
     where
       v :: Defn V
       v = lift vUnsafe
       e1 :: Defn Exp
       e1 = lift e1Unsafe
       e2 :: Defn Exp
       e2 = lift e2Unsafe

       env_bind = mapSingleton `app` v `mapp` (go `mapp` env `app` e1)
       env' = lub env_bind env
   LetRec bindsUnsafe eUnsafe -> go `mapp` env' `app` e
     where
       e :: Defn Exp
       e = lift eUnsafe
       binds :: Defn (M.Map V Exp)
       binds = lift bindsUnsafe

       step :: Defn (Env -+> Env)
       step = mlam $ \env ->
         let  env_bind = mapWithKey (lam $ \v -> lam $ \e -> go `mapp` env `app` e) binds
         in   lub env env_bind
       env' = fix step

someVal :: Exp
someVal = Lam "y" (Var "y")

example1 :: Exp
example1 = Let "x" Throw someVal

example2 :: Exp
example2 = Let "x" Throw (App (Var "x") someVal)

example3 :: Exp
example3 = LetRec (M.fromList [("x", Throw)]) (App (Var "x") someVal)

example4 :: Exp
example4 = LetRec (M.fromList [("x", App (Var "x") Throw)]) (App (Var "x") someVal)

example5 :: Exp
example5 = LetRec (M.fromList [("x", App (Var "x") someVal)]) (App (Var "x") someVal)

example6 :: Exp
example6 = LetRec (M.fromList [("x", Var "x")]) (App (Var "x") someVal)

