{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures, RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE IncoherentInstances, UndecidableInstances #-}

module Hatafun where
import Prelude hiding ((^^), (^), mempty, fst, snd, and, not, length)
import Data.Set hiding (split, take, drop)

data Nat =  Z | S Nat deriving (Eq, Ord)


-- Monotone functions
newtype a -+> b = MFun { unMFun :: a -> b }
infixr 8 -+>

class MetaSemiLattice a where
    meta_bot :: a
    meta_lub :: a -> a -> a

class SemiLattice a where
    bot :: Defn a
    lub :: MonoOps repr => repr v s a -> repr v s a -> repr v s a

instance MetaSemiLattice Int where
    meta_bot = 0
    meta_lub = max

instance MetaSemiLattice a => SemiLattice a where
    bot = lift meta_bot
    lub x y = lift $ meta_lub (unlift x) (unlift y)

-- equality testing is not monotone
class EqType a where
    eq :: MonoOps repr => repr v 'Z a -> repr v 'Z a -> repr v s Bool

class OrdType a where
    leq :: MonoOps repr => repr v s a -> repr v 'Z a -> repr v s Bool

instance Ord a => OrdType a where
    leq a b = lift $ (unlift a) <= (unlift b)

instance Eq a => EqType a where
    eq x y = lift $ (unlift x) == (unlift y)

class FinType a where
    elements :: [a]

class (FinType a, SemiLattice a) => FiniteSemiLattice a where
    top :: a

powerset :: Ord a => [a] -> [Set a]
powerset [] = [Data.Set.empty]
powerset (x:xs) = [Data.Set.insert x ps | ps <- powerset xs] ++ powerset xs

instance (FinType a, Ord a) => FinType (Set a) where
    elements = powerset elements

instance FinType a => FinType (a, a) where
    elements = [(x, y) | x <- elements, y <- elements]

instance (FinType a, Ord a, SemiLattice (Set a)) => FiniteSemiLattice (Set a) where
    top = Data.Set.fromList $ elements

instance MetaSemiLattice Nat where
    meta_bot = Z
    meta_lub = mx
        where mx Z n = n
              mx n Z = n
              mx (S n) (S m) = S $ mx n m

instance MetaSemiLattice Bool where
    meta_bot = False
    meta_lub = (||)

instance FinType Bool where
    elements = [True, False]

-- instance FiniteSemiLattice Bool where
--     top = True

-- Ord is required by Set
instance Ord a => MetaSemiLattice (Set a) where
    meta_bot = empty
    meta_lub = union

instance MetaSemiLattice a => MetaSemiLattice (a, a) where
    meta_bot = (meta_bot, meta_bot)
    meta_lub (a, b) (c, d) = (meta_lub a c, meta_lub b d)

type MVar repr (v :: Nat) (s :: Nat) a =
    forall (v' :: Nat) (s' :: Nat).
    (MonoOps repr, PLUS_GE v s' v') =>
    repr v' s' a

type Var repr (v :: Nat) (s :: Nat) a =
    forall (v' :: Nat) (s' :: Nat).
    (MonoOps repr) =>
    repr v' s' a

class MonoOps (repr :: Nat -> Nat -> * -> *) where
    unlift :: repr v s a -> a
    lift :: a -> repr v s a
    fix :: (Eq a, FiniteSemiLattice a) => repr v s (a -+> a) -> repr v s a
    fix' :: (Eq a, MetaSemiLattice a, SemiLattice a) => repr v s a -> repr v s (a -+> a) -> repr v s a
    lam :: (Var repr v s a -> repr v s b) -> repr v s (a -> b)
    mlam :: (MVar repr v s a -> repr ('S v) ('S s) b) -> repr v s (a -+> b)
    app :: repr v s (a -> b) -> repr v 'Z a -> repr v s b
    mapp :: repr v s (a -+> b) -> repr v s a -> repr v s b
    merase :: repr v s (a -+> b) -> repr v s (a -> b)
    test :: repr v 'Z Bool -> repr v s a -> repr v s a -> repr v s a
    when :: SemiLattice a => repr v s Bool -> repr v s a -> repr v s a

newtype Mono (v :: Nat) (s :: Nat) a = Mono { unMono :: a } deriving (Show, Eq)
instance MonoOps Mono where
    unlift = unMono
    lift = Mono
    fix f =
        let fixpoint mf x = let x' = mf x
                            in if unMono $ eq (Mono x) (Mono x') 
                                then x
                                else fixpoint mf x'
        in Mono $ fixpoint (unMFun (unMono f)) (unMono bot)
    fix' v f = 
        let fixpoint mf x = let x' = mf x
                            in if v /= lub v (Mono x') then unMono v
                            else if x == x' then x
                            else fixpoint mf x'
        in Mono $ fixpoint (unMFun (unMono f)) (unMono bot)
    lam f = Mono $ \x -> unMono (f (Mono x))
    mlam f = Mono $ MFun (\x ->  unMono (f (Mono x)))
    app f a = Mono $ (unMono f) (unMono a)
    mapp f a = Mono $ (unMFun.unMono $ f) (unMono a)
    merase f = Mono (unMFun.unMono $ f)
    test cond thn els = if unMono cond then thn else els
    when cond res = if unMono cond then res else bot

λ :: MonoOps repr => (Var repr v s a -> repr v s b) -> repr v s (a -> b)
λ = lam
λ' :: MonoOps repr => (MVar repr v s a -> repr ('S v) ('S s) b) -> repr v s (a -+> b)
λ' = mlam
(^) :: MonoOps repr => repr v s (a -> b) -> repr v 'Z a -> repr v s b
(^) = app
(^^) :: MonoOps repr => repr v s (a -+> b) -> repr v s a -> repr v s b
(^^) = mapp

class PLUS_GE (v :: Nat) (s :: Nat) (v' :: Nat)
instance PLUS_GE a b 'Z
instance PLUS_GE a b a
instance (PLUS_GE v s v') => PLUS_GE v ('S s) ('S v')
instance (PLUS_GE v s v') => PLUS_GE ('S v) s ('S v')

-- Hiding scoping information
type Defn a =
    forall repr (v :: Nat) (s :: Nat).
    MonoOps repr =>
    repr v s a

-- Turn unlift into a monomorphic version, so that
-- type checker could infer
unsafeUnlift :: MonoOps repr => repr 'Z 'Z a -> a
unsafeUnlift = unlift 

-- Product operators
-- These operations could also be defined in a class as an atomic operation
pair :: MonoOps repr => repr v s a -> repr v s b -> repr v s (a, b)
pair a b = lift (unlift a, unlift b)

fst :: MonoOps repr => repr v s (a, b) -> repr v s a
fst p = lift (fst' (unlift p))
    where fst' (a, _) = a

snd :: MonoOps repr => repr v s (a, b) -> repr v s b
snd p = lift (snd' (unlift p))
    where snd' (_, b) = b

-- Sum operators
left :: MonoOps repr => repr v s a  -> repr v s (Either a b)
left a = lift $ Left $ unlift a

right :: MonoOps repr => repr v s b  -> repr v s (Either a b)
right a = lift $ Right $ unlift a

split :: MonoOps repr => repr v 'Z (Either a b) -> repr v s (a -> c) -> repr v s (b -> c) -> repr v s c
split e l r = lift $ split_at (unlift e) (unlift l) (unlift r)
    where
        split_at (Left a) lf _ = lf a
        split_at (Right b) _ rf = rf b
mono_split :: MonoOps repr => repr v s (Either a b) -> repr v s (a -+> c) -> repr v s (b -+> c) -> repr v s c
mono_split e l r = lift $ split_at (unlift e) (unMFun (unlift l)) (unMFun (unlift r))
    where
        split_at (Left a) lf _ = lf a
        split_at (Right b) _ rf = rf b

-- Bool operators
tt :: Defn Bool
tt = lift True

ff :: Defn Bool
ff = lift False

-- Set operators
set :: (Ord a) => [a] -> Defn (Set a)
set xs = lift (Data.Set.fromList xs)

range :: MonoOps repr => repr v 'Z Int -> repr v s Int -> repr v s (Set Int)
range lo hi = lift $ Data.Set.fromList [(unlift lo)..(unlift hi)]

mbind :: (Ord a, Ord b, MonoOps repr) =>
    repr v s (Set a) -> repr v s (a -> Set b) -> repr v s (Set b)
mbind sa f = lift $ unions $ Data.Set.map (unlift f) (unlift sa)

munion :: (Ord a, MonoOps repr) =>
    repr v s (Set a) -> repr v s (Set a) -> repr v s (Set a)
munion sa f = lift $ Data.Set.union (unlift f) (unlift sa)

mempty :: (Ord a) => Defn (Set a)
mempty = lift $ Data.Set.empty

msingleton :: (Ord a, MonoOps repr) => repr v 'Z a -> repr v s (Set a)
msingleton x = lift $ Data.Set.singleton (unlift x)

mmap :: (Ord a, Ord b) => Defn ((a -> b) -> Set a -+> Set b)
mmap = lam $ \f -> mlam $ \x ->
    mbind x (lam $ \e -> msingleton (f `app` e))

mfilter :: Ord a => Defn ((a -> Bool) -+> Set a -+> Set a)
-- failed because this is not monotone (mempty can be replaced with anything)
-- our type system finds it!
-- mfilter = mlam $ \f -> mlam $ \x ->
--     mbind `mapp` (lam $ \e ->
--         test (f `app` e) (msingleton `app` e) (mempty)) `mapp` x
mfilter = mlam $ \f -> mlam $ \x ->
    mbind x (lam $ \e ->
        when (f `app` e) (msingleton e))

(><) :: (Ord a, Ord b, MonoOps repr) => repr v s (Set a) -> repr v s (Set b) -> repr v s (Set (a, b))
a >< b = mbind a $ lam $ \x ->
    mbind b $ lam $ \y ->
        msingleton (pair x y)

comp :: (Ord a, Ord b, Ord c, Eq b, MonoOps repr) =>
    repr v s (Set (a, b)) -> repr v s (Set (b, c)) -> repr v s (Set (a, c))
comp a b = mbind a $ lam $ \x ->
        mbind b $ lam $ \y ->
            let xa = fst x
                xb = snd x
                yb = fst y
                yc = snd y in
            when (eq xb yb) $
                msingleton (pair xa yc)

-- string 
str :: MonoOps repr => String -> repr v s String
str = lift

length :: MonoOps repr => repr v 'Z String -> repr v s Int
length s = lift $ length' $ unlift s
    where length' [] = 0
          length' (x:xs) = 1 + (length' xs)

substring :: MonoOps repr => repr v 'Z String -> repr v 'Z Int -> repr v 'Z Int -> repr v 'Z String
substring s lo hi = lift $ drop (unlift lo) . take (unlift hi) $ (unlift s)

int :: Int -> Defn Int
int i = lift i

plus :: Defn (Int -+> Int -+> Int)
plus = mlam $ \a -> mlam $ \b -> lift $ (unsafeUnlift a) + (unsafeUnlift b)

minus :: Defn (Int -+> (Int -> Int))
minus = mlam $ \a -> lam $ \b -> lift $ (unsafeUnlift a) - (unsafeUnlift b)

-- tests

data Person = Earendil | Elrond | Arwen deriving (Eq, Ord, Show)
instance MetaSemiLattice Person where
    meta_bot = Earendil
    meta_lub = max
instance FinType Person where
    elements = [Earendil, Elrond, Arwen]
instance FiniteSemiLattice Person where
    top = Arwen

parent :: Defn (Set (Person, Person))
parent = set [(Earendil, Elrond), (Elrond, Arwen)]
ancestor :: Defn (Set (Person, Person))
ancestor = fix $ mlam $ \x ->
    lub parent (x `comp` x)

persons :: Defn (Set String)
persons = set ["earendil", "elrond", "arwen"]
parent' :: Defn (Set (String, String))
parent' = set [("earendil", "elrond"), ("elrond", "arwen")]
ancestor' :: Defn (Set (String, String))
ancestor' = fix' (persons >< persons) $ mlam $ \x -> lub parent' (x `comp` x)

trans :: (Eq a, Ord a) => Defn (Set a -+> Set (a, a) -+> Set (a, a))
trans = mlam $ \v -> mlam $ \e ->
    fix' (v >< v) $ mlam $ \s -> lub e (s `comp` s)

ancestor'' :: Defn (Set (String, String))
ancestor'' = trans `mapp` persons `mapp` parent'

type Sym = String
type Rule = Either String (Sym, Sym)
type Grammar = Set (Sym, Rule)
type Fact = (Sym, (Int, Int))

and :: Defn (Bool -+> Bool -+> Bool)
and = mlam $ \x -> mlam $ \y -> when x (when y tt)
not :: Defn (Bool -> Bool)
not = lam $ \x -> test x ff tt

iter :: Defn (String -> Grammar -+> Set Fact -+> Set Fact)
iter = lam $ \text -> mlam $ \grammar -> mlam $ \chart ->
    let n = length text
    in mbind grammar $ lam $ \p ->
        let a = fst p
        in split (snd p)
            (lam $ \s ->
                let l = length s
                in mbind (range (int 0) (minus `mapp` n `app` l)) $ lam $ \i ->
                    when (eq s $ substring text i (plus `mapp` i `mapp` l)) $
                        msingleton $ pair a $ pair i (plus `mapp` i `mapp` l))
            (lam $ \p ->
                let b = fst p
                    c = snd p
                in mbind chart (lam $ \p ->
                    let b' = fst p
                        i = fst $ snd p
                        j = snd $ snd p
                    in when (eq b b') $
                            mbind chart $ lam $ \p ->
                                let c' = fst p
                                    j' = fst $ snd p
                                    k = snd $ snd p
                                in when (and `mapp` (eq c c') `mapp` (eq j j')) $
                                    msingleton $ pair a $ pair i k))

parse :: Defn (String -> Grammar -+> Set Sym)
parse = lam $ \text -> mlam $ \grammar ->
    let n = length text
        bound = mbind grammar $ lam $ \p -> let a = fst p in
            mbind (range (int 0) n) $ lam $ \i ->
                mbind (range i n) $ lam $ \j ->
                    msingleton $ pair a $ pair i j
        chart = fix' bound $ mlam $ \c -> iter `app` text `mapp` grammar `mapp` c
    in
        mbind chart $ lam $ \p ->
            when (eq (int 0) (fst (snd p))) $
                when (eq n (snd (snd p))) $
                    msingleton (fst p)

grammar :: Defn Grammar
grammar = set [
    ("(", Left "("),
    (")", Left ")"),
    ("b", Right ("(", ")")),
    ("b", Right ("b", "b")),
    ("b", Right ("(", "b_)")),
    ("b_)", Right ("b", ")"))]
test_parse_pass :: Defn (Set Sym)
test_parse_pass = parse `app` (str "((()())())") `mapp` grammar

test_parse_fail :: Defn (Set Sym)
test_parse_fail = parse `app` (str "()(") `mapp` grammar

eval :: Defn a -> a
eval program = unMono program

main = do
    print $ eval ancestor
    print $ eval ancestor'
    print $ eval ancestor''
    print $ eval test_parse_fail
    print $ eval test_parse_pass
