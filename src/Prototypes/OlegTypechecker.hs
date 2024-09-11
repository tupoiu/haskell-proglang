module OlegTypechecker where

type Name = String
data Term = V Name
          | L Name Typ Term -- Annotated lambdas!
          | A Term Term
          | I Int
          | IFZ Term Term Term
          | Term :+ Term
          | Fix Term
          deriving (Show, Eq)

-- We interpret to either an int or a closure
data Value = VI Int | VC (Value -> Value)
instance Show Value where
  show (VI x) = "VI " <> show x
  show (VC _) = "<function>"

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
  L nx _ out -> VC (\vx -> eval (ext env nx vx) out) -- Evaluate out with nx as vx
  A vf vx -> case eval env vf of
    VC f -> f (eval env vx)
    _ -> error $ "Type error: cannot apply an Int"
  I vx -> VI vx
  IFZ p e1 e2 -> case eval env p of
    VI 0 -> eval env e1
    VI _ -> eval env e2
    _ -> error "If zero doesn't work with a function!"
  e1 :+ e2 -> case (eval env e1, eval env e2) of
    (VI x1, VI x2) -> VI (x1 + x2)
    _ -> error "Cannot add anything but integers"
  Fix vf -> case eval env vf of -- fix :: (a -> b) -> (a -> b)
    -- Fix is a weird operator
    -- fix (0:) = 0:0:0:0:... = 0:(fix (0:))
    -- fix f = f (fix f)
    -- fix = \f -> f (fix f)
    VI _ -> error "Cannot Fix a non-function"
    VC f -> f (eval env (Fix vf))


example :: Term
example = (L "x" TInt (IFZ (V "x") (I 1) ((V "x") :+ (I 2)))) `A` (I 10)

decr :: Term
decr = (L "u" TInt ((V "u") :+ (I (-1))))

-- x :: Int -> Int -> Int
-- multiply' :: Int -> Int -> Int
multiply' a b = (let x = f x in x) a
  where f g ct = if ct == 0 then 0 else b + g (ct - 1)

mult = (L "a" TInt (L "b" TInt (
  Fix (L "g" (TInt :> TInt) (L "ct" TInt 
    (IFZ (V "ct") (I 0) ((V "b") :+ (A (V "g") ((V "ct") :+ (I (-1)))))))) `A` (V "a")
  )))

data Typ = TInt
         | Typ :> Typ
         deriving (Show, Eq)

type TEnv = [(Name, Typ)]

tLkup :: TEnv -> Name -> Typ
tLkup env nx = case lookup nx env of
  Just tx -> tx
  Nothing -> error $ "Unbound variable in typechecking: " <> nx

tExt :: TEnv -> Name -> Typ -> TEnv
tExt env nx tx = (nx, tx) : env

tEval :: TEnv -> Term -> Typ
tEval env t = case t of
  V nx -> tLkup env nx
  L nx tx out -> let tOut = tEval (tExt env nx tx) out in tx :> tOut
  A vf vx -> 
    let
      tf = tEval env vf
      tx = tEval env vx
    in case (tf, tx) of
      (e_tx :> tOut, a_tx) | e_tx == a_tx -> tOut
      _ -> error $ "Type error: Cannot apply " <> show tf <> " to a value of type " <> show tx
  I _ -> TInt
  IFZ vp vx vy ->
    let
      tp = tEval env vp
      tx = tEval env vx
      ty = tEval env vy
    in case tp of
      TInt | tx == ty -> tx
      TInt -> error $ "Type error: If branches return two different types, " <> show tx <> " and " <> show ty
      _ -> error $ "TypeError: Ifzero takes in an Int"
  vx :+ vy -> 
    let (tx, ty) = (tEval env vx, tEval env vy)
     in case (tx, ty) of
    (TInt, TInt) -> TInt
    _ -> error $ "Type error: Cannot add " <> show tx <> " and " <> show ty
  Fix vf -> case tEval env vf of
    ((ta :> tb) :> (ta' :> tb')) | ta == ta' && tb == tb' -> ta :> tb
    tf -> error $ "Fix :: ((a -> b) -> (a -> b)) -> (a -> b) can't accept " <> show tf
