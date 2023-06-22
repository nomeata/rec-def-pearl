{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Example (removeCatch,example1, example2, example3, example4, example5, example6, example7) where

import Datafix
import Data.Proxy (Proxy (..))
import qualified Data.Map as M
import Datafix.Utils.SemiLattice (JoinSemiLattice (..), BoundedJoinSemiLattice (..))

type V = String
data ExpF r  =  VarF V | LamF V r | AppF r r | ThrowF | CatchF r | LetRecF [(V, r)] r
data Exp = Fix (ExpF Exp)


deriving instance Show e => Show (ExpF e)
deriving instance Show RWE
instance Show Exp where
  show (Fix e) = "(" ++ show e ++ ")"

data RBool = RFalse | RTrue deriving (Eq, Ord, Show)
instance JoinSemiLattice RBool where
  (\/) = max
instance BoundedJoinSemiLattice RBool where
  bottom = RFalse

data RWE = MkRWE !RBool Exp
instance Eq RWE where
  MkRWE b1 _ == MkRWE b2 _ = b1 == b2
instance JoinSemiLattice RWE where
  MkRWE b1 _ \/ MkRWE b2 e2 = MkRWE (b1 \/ b2) e2
instance BoundedJoinSemiLattice RWE where
  bottom = MkRWE bottom undefined


type TransferAlgebra lattice = forall m. Monad m =>
  Proxy m -> Proxy lattice -> M.Map V (m lattice) -> ExpF (m lattice) -> m lattice

type TF m = m (Domain m)

buildDenotation :: forall domain. Eq domain => IsBase domain ~ True =>
  TransferAlgebra domain  -> Exp -> Denotation domain domain
buildDenotation alg exp = go
  where
    go :: forall m. MonadDatafix m => domain ~ Domain (DepM m) => m (TF (DepM m))
    go = buildDenotation' alg exp

buildDenotation' :: forall domain m. MonadDatafix m => domain ~ Domain (DepM m) =>
  Eq domain => IsBase domain ~ True => TransferAlgebra domain -> Exp -> m (TF (DepM m))
buildDenotation' alg' = buildExpr M.empty
  where
    alg = alg' (Proxy :: Proxy (DepM m)) (Proxy :: Proxy domain)
    buildExpr :: M.Map V (TF (DepM m)) -> Exp -> m (TF (DepM m))
    buildExpr !env (Fix expr) =
      case expr of
        VarF id_ -> pure (alg env (VarF id_))
        LamF id_ body -> do
          transferBody <- buildExpr (M.insert id_ (pure bottom) env) body
          pure (alg env (LamF id_ transferBody))
        AppF f a -> do
          transferF  <- buildExpr env f
          transferA  <- buildExpr env a
          pure (alg env (AppF transferF transferA))
        ThrowF -> pure (alg env ThrowF)
        CatchF e -> do
          transferE <- buildExpr env e
          pure (alg env (CatchF transferE))
        LetRecF bind body -> do
          (env', transferredBind) <- datafixBindingGroup env bind
          transferBody <- buildExpr env' body
          pure (alg env (LetRecF transferredBind transferBody))

    datafixBindingGroup !env binders = case binders of
      [] -> pure (env, [])
      ((id_, rhs):binders') ->
        datafixEq $ \self -> do
          let env' = M.insert id_ self env
          (env'', transferredBind) <- datafixBindingGroup env' binders'
          transferRHS <- buildExpr env'' rhs
          pure ((env'', (id_, self):transferredBind), transferRHS)

transferFunctionAlg :: TransferAlgebra RWE
transferFunctionAlg _ _ env e = case e of
  VarF id_ -> do
    MkRWE throws _ <- env M.! id_
    pure (MkRWE throws (Fix $ VarF id_))
  ThrowF -> pure (MkRWE RTrue (Fix $ ThrowF))
  CatchF e -> do
    MkRWE can_throw e <- e
    let  new_e  | RTrue <- can_throw  = Fix $ CatchF e
                | otherwise           = e
    pure (MkRWE RFalse new_e)
  LamF id_ body -> do
    MkRWE can_throw body <- body
    pure (MkRWE can_throw (Fix $ LamF id_ body))
  AppF f a -> do
    MkRWE throw_f f <- f
    MkRWE throw_a a <- a
    let  RFalse ||| b = b
         RTrue  ||| _ = RTrue
    pure (MkRWE (throw_f ||| throw_a) (Fix $ AppF f a))
  LetRecF bind body -> do
    MkRWE can_throw body <- body
    let  extract_rhs (v,me) = do
           MkRWE _can_throw e <- me
           pure (v, e)
    bind' <- mapM extract_rhs bind
    pure (MkRWE can_throw (Fix $ LetRecF bind' body))

removeCatch :: Exp -> Exp
removeCatch e =
  let MkRWE _ e' = evalDenotation @RWE (buildDenotation transferFunctionAlg e) NeverAbort
  in e'

pattern LetRec bind body = Fix (LetRecF bind body)
pattern App f a = Fix (AppF f a)
pattern Lam x e = Fix (LamF x e)
pattern Catch e = Fix (CatchF e)
pattern Throw = Fix ThrowF
pattern Var x = Fix (VarF x)

someVal :: Exp
someVal = Lam "y" (Var "y")

example1 :: Exp
example1 = LetRec [("x", Throw)] someVal

example2 :: Exp
example2 = LetRec [("x",Throw)] (App (Var "x") someVal)

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
