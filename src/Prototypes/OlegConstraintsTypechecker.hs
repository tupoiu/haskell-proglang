module Prototypes.OlegConstraintsTypechecker where

import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace(trace)

type Name = String

-- We interpret to either an int or a closure
data Value = VI Int | VC (Value -> Value)
instance Show Value where
  show (VI x) = "VI " <> show x
  show (VC _) = "<function>"

data Term = V Name
          | L Name Term
          | A Term Term
          | I Int
          | Builtin Typ Value
          | Let Name Term Term
          -- | IFZ Term Term Term
          -- | Term :+ Term
          -- | Fix Term
          deriving (Show)

instance Eq Term where
  -- Default implementation for all constructors
  (==) :: Term -> Term -> Bool
  (V n1) == (V n2) = n1 == n2
  (L n1 t1) == (L n2 t2) = n1 == n2 && t1 == t2
  (A t1 t2) == (A t3 t4) = t1 == t3 && t2 == t4
  (I i1) == (I i2) = i1 == i2
  (Let nx vx vout) == (Let ny vy vout') = nx == ny && vx == vy && vout == vout'
  -- (IFZ t1 t2 t3) == (IFZ t4 t5 t6) = t1 == t4 && t2 == t5 && t3 == t6
  (Builtin _ _) == _ = error "Can't compare builtin"
  _ == (Builtin _ _) = error "Can't compare builtin"
  _ == _ = False

type Env = [(Name, Value)] -- Note that Env contains evaluated values, not terms

env0 :: Env
env0 = []

-- Lookup variable in the env at runtime
lkup :: Env -> Name -> Value
lkup env nx = case lookup nx env of
  Just x -> x
  Nothing -> error $ "Unbound variable: " <> nx

-- Add to the env
ext :: Env -> Name -> Value -> Env
ext env nx vx = (nx, vx) : env

eval :: Env -> Term -> Value
eval env expr = case expr of
  V nx -> lkup env nx
  L nx out -> VC (\vx -> eval (ext env nx vx) out) -- Evaluate out with nx as vx
  A vf vx -> case eval env vf of
    VC f -> f (eval env vx)
    _ -> error $ "Type error: cannot apply an Int"
  I vx -> VI vx
  Builtin _ vx -> vx
  Let nx vx out -> eval (ext env nx (eval env vx)) out
  -- IFZ p e1 e2 -> case eval env p of
  --   VI 0 -> eval env e1
  --   VI _ -> eval env e2
  --   _ -> error "If zero doesn't work with a function!"
  -- e1 :+ e2 -> case (eval env e1, eval env e2) of
  --   (VI x1, VI x2) -> VI (x1 + x2)
  --   _ -> error "Cannot add anything but integers"
  -- Fix vf -> case eval env vf of -- fix :: (a -> b) -> (a -> b)
  --   -- Fix is a weird operator
  --   -- fix (0:) = 0:0:0:0:... = 0:(fix (0:))
  --   -- fix f = f (fix f)
  --   -- fix = \f -> f (fix f)
  --   VI _ -> error "Cannot Fix a non-function"
  --   VC f -> f (eval env (Fix vf))


-- example :: Term
-- example = (L "x" TInt (IFZ (V "x") (I 1) ((V "x") :+ (I 2)))) `A` (I 10)

-- Idea: Make builtins be generated from a [(Name, Typ, Value)]

plus :: Term
plus = Builtin (TInt :> (TInt :> TInt)) plusValue
  where
    plusValue = VC $ \x -> VC $ \y -> case (x, y) of
      (VI vx, VI vy) -> VI (vx + vy)
      _ -> error "Plus: Impossible"

decr :: Term
decr = L "u" (plus `A` V "u" `A` I (-1))

-- x :: Int -> Int -> Int
multiply' :: Int -> Int -> Int
multiply' a b = (let x = f x in x) a
  where f g ct = if ct == 0 then 0 else b + g (ct - 1)

-- mult = (L "a" TInt (L "b" TInt (
  -- Fix (L "g" (TInt :> TInt) (L "ct" TInt 
    -- (IFZ (V "ct") (I 0) ((V "b") :+ (A (V "g") ((V "ct") :+ (I (-1)))))))) `A` (V "a")
  -- )))

data Typ = TInt
         | Typ :> Typ
         | TV TVarName
         deriving (Show, Eq)

type TVarName = Int
-- Type variable environment
data TVE = TVE Int (Map TVarName Typ) deriving (Show, Eq)

newTV :: TVE -> (Typ, TVE)
newTV (TVE n tvenv) = (TV n, TVE (n+1) tvenv)

-- Initial type variable environment
tvenv0 :: TVE
tvenv0 = TVE 0 mempty

tvLookup :: TVE -> TVarName -> Maybe Typ
tvLookup (TVE _ tvenv) tvx = Map.lookup tvx tvenv

-- We know that type variable tvx is of type tx
tvExt :: TVE -> TVarName -> Typ -> TVE
tvExt (TVE n tvmap) tvx tx = TVE n (Map.insert tvx tx tvmap)

-- If tx = TV 1, and tvenv = [(1, TV 2), (2, TInt)] then we return TInt
tvsub :: TVE -> Typ -> Typ
tvsub _ TInt = TInt
tvsub tve (tx :> ty) = (tvsub tve tx) :> (tvsub tve ty)
tvsub tve (TV n) = case tvLookup tve n of
  Just t -> t
  Nothing -> TV n

-- Substitute the expressions into the equations
-- e.g.
-- unify t1 t4 tvenv = tvenv'
-- t1 = TV 1, t4 = TV 4, tvenv = [(1, TV 3), (3, TV 2 :> TV 2)]
-- will return
-- [(1, TV 2 :> TV 2), (3, TV 2 :> TV 2), (4, TV 2 :> TV 2)]
unify :: Typ -> Typ -> TVE -> Either String TVE
unify tx ty tve = unify' (tvsub tve tx) (tvsub tve ty) tve

-- unify' should loop through all of the entries of the tve and set their variables to match

-- Assumptions about unify:
-- 1. The equations that are in TVE are already solved.
-- 2. No types in the TVE reference themself e.g. t1 = t1 is not allowed (DAG)

unify' :: Typ -> Typ -> TVE -> Either String TVE
unify' TInt TInt tve = Right tve
unify' TInt tx tve = unify' tx TInt tve -- Symmetric (WLOG Int doesn't come first)
unify' (ta :> tb) (tc :> td) tve = do
  tve' <- unify ta tc tve
  tve'' <- unify tb td tve'
  pure tve''
unify' (_ :> _) TInt tve = Left $ "Cannot assign type Int to a function (arrow)" <> show tve
unify' (ta :> tb) tc tve = unify' tc (ta :> tb) tve -- Symmetric 
-- Check whether tx has already been defined.
unify' (TV tvx) TInt tve = case tvLookup tve tvx of
    Just TInt -> Right tve
    Just (_ :> _) -> Left "Cannot assign type Int to a function (lookup)"
    Just (TV tvy) -> do
      tve' <- tvReplace tve tvx TInt
      unify (TV tvy) TInt tve'
    Nothing -> tvReplace tve tvx TInt
unify' (TV tvx) (TV tvy) tve = tvReplace tve tvx (TV tvy) -- Substitute tvx wherever tvy appears
unify' (TV tvx) (ta :> tb) tve = tvReplace tve tvx (ta :> tb)

-- foreach (tnu, tu) in tve, return (tnu, tvsub tvx tvy tve)
tvChase :: TVarName -> Typ -> Typ -> Maybe Typ
tvChase tvx ty tu = case tu of
  (TV tvu) | tvu == tvx -> Just ty
  (ta :> tb) -> case (tvChase tvx ty ta, tvChase tvx ty tb) of
    (Nothing, Nothing) -> Nothing
    (Just ta', Nothing) -> Just $ ta' :> tb
    (Nothing, Just tb') -> Just $ ta :> tb'
    (Just ta', Just tb') -> Just $ ta' :> tb'
  _ -> Nothing

tvReplace :: TVE -> TVarName -> Typ -> Either String TVE
-- Look at only the entries which contain tvx and sub tvy into those entries
tvReplace (TVE n tvmap) tvx ty
  | tvOccurs (TVE n tvmap) tvx ty = Left "Cannot have a type which depends on itself"
  | otherwise = Right $ TVE n (tvReplace' tvmap tvx ty)


tvReplace' :: Map TVarName Typ -> TVarName -> Typ -> Map TVarName Typ
-- Look at only the entries which contain tvx and sub tvy into those entries
tvReplace' tvmap tvx ty = case Map.lookup tvx tvmap of
    Nothing -> Map.insert tvx ty newMap
    Just _ -> newMap
  where
    newMap = Map.fromList $ map f (Map.toList tvmap)
    -- f is a function to swap all occurences of tvx with tvy
    f :: (TVarName, Typ) -> (TVarName, Typ)
    -- f (tvu, tu) | tvu == tvx = (tvu, tu)
    f (tvu, tu) = case tvChase tvx ty tu of
      Nothing -> (tvu, tu)
      Just tu' -> (tvu, tu')

tvOccurs :: TVE -> TVarName -> Typ -> Bool
tvOccurs tve tvx ty = case ty of
  TInt -> False
  (ta :> tb) -> tvOccurs tve tvx ta || tvOccurs tve tvx tb
  (TV tvz) -> case tvLookup tve tvz of
    Nothing -> False
    Just tz -> tvOccurs tve tvx tz


exampleTVE :: TVE
exampleTVE = TVE 4 $ Map.fromList [(1, TV 2), (3, TV 2 :> TV 2)]

testUnify1 :: Either String TVE
testUnify1 = unify (TV 1) (TV 4) exampleTVE
-- Expected: [(1, TV 4), (2, TV 4), (3, TV 4 :> TV 4)]

testUnify2 :: Either String TVE
testUnify2 = unify (TV 2) (TV 4) tve
  where tve = TVE 5 $ Map.fromList [(1, TV 2 :> TV 2), (3, TV 2 :> TV 2)]
-- Expected: t2 = t4, t1 = t4 :> t4, t3 = t4 :> t4

testUnify3 :: Either String TVE
testUnify3 = unify TInt (TV 2 :> TV 2) exampleTVE

type TEnv = [(Name, Typ)]

tLkup :: TEnv -> Name -> Typ
tLkup env nx = case lookup nx env of
  Just tx -> tx
  Nothing -> error $ "Unbound variable in typechecking: " <> nx

tExt :: TEnv -> Name -> Typ -> TEnv
tExt env nx tx = (nx, tx) : env

tEval :: TEnv -> Term -> TVE -> (Typ, TVE)
tEval env vx tve0 = (tvsub tve tx, tve)
  where (tx, tve) = tEval' env vx tve0

tEval' :: TEnv -> Term -> TVE -> (Typ, TVE)
tEval' env t tve0 = case t of
  V nx -> (tLkup env nx, tve0)
  L nx out ->
    let
      -- Make a new TV for the name in the lambda
      (tx, tve1) = newTV tve0
      -- Unify the type of this type variable in out
      -- By using tExt only in the inside of the lambda, we prevent leaking the name x
      -- i.e. x is seen as undeclared outside of this lambda
      (tOut, tve2) = tEval' (tExt env nx tx) out tve1
     in (tx :> tOut, tve2)
  A vf vx ->
    let
      -- Unify the types of the two terms
      (tf, tve1) = tEval' env vf tve0
      (tx, tve2) = tEval' env vx tve1
      -- We now have 2 types and an up to date type environment.
      -- Set tf = tx :> ?
    in
      case tf of
        (e_tx :> tOut) -> case unify e_tx tx tve2 of
          Left e -> error e
          Right tve -> (tOut, tve)
        TInt -> error "Can't apply int"
        TV tvf ->
          let
            -- Create a new type var for the output of f
            (tOut, tve') = newTV tve2
            -- tf = tx :> tOut
            tve'' = case unify (TV tvf) (tx :> tOut) tve' of
              Left e -> error e
              Right tve -> tve
          in (tOut, tve'')
  I _ -> (TInt, tve0)
  Builtin tx _ -> (tx, tve0)
  Let nx vx out ->
    let
      -- Evaluate the type of the variable
      (tx, tve1) = tEval' env vx tve0
      -- We need to instantiate the type of the variable wherever it is in the output,
      -- so we need to return an "alteration" which can instantiate our let
      (tOut, tve2) = tEval' (tExt env nx tx) out tve1
    in (tOut, tve2)
  -- IFZ vp vx vy ->
  --   let
  --     tp = tEval' env vp
  --     tx = tEval' env vx
  --     ty = tEval' env vy
  --   in case tp of
  --     TInt | tx == ty -> tx
  --     TInt -> error $ "Type error: If branches return two different types, " <> show tx <> " and " <> show ty
  --     _ -> error $ "TypeError: Ifzero takes in an Int"
  -- vx :+ vy ->
  --   let
  --     (tx, tve1) = tEval' env vx tve0
  --     (ty, tve2) = tEval' env vy tve1
  --     tve3 :: TVE
  --     tve3 = case unify tx TInt =<< unify ty TInt tve2 of
  --       Left e -> error e
  --       Right tve -> tve
  --   in (TInt, tve3)
    -- _ -> error $ "Type error: Cannot add " <> show tx <> " and " <> show ty
  -- Fix vf -> case tEval' env vf of
    -- ((ta :> tb) :> (ta' :> tb')) | ta == ta' && tb == tb' -> ta :> tb
    -- tf -> error $ "Fix :: ((a -> b) -> (a -> b)) -> (a -> b) can't accept " <> show tf

tE :: Term -> (Typ, TVE)
tE t = tEval [] t tvenv0

tE' :: Term -> (Typ, TVE)
tE' t = tEval' [] t tvenv0

tEnvFromList :: [(TVarName, Typ)] -> TVE
tEnvFromList = TVE (-999) . Map.fromList
