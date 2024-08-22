module LambdaCalc where

import Debug.Trace

type Name = String
data Term = Var Name
          | Lam Name Term
          | Apl Term Term
          | Lit Literal
          | Let Name Term Term
          | Func (Term -> Term)
          deriving (Eq)

instance Eq (Term -> Term) where
    _ == _ = error "Cannot compare functions"

instance Show Term where
    show term = case term of
        Var name -> "Var (" ++ show name ++ ")"
        Lam name term' -> "Lam (" ++ show name ++ ") (" ++ show term' ++ ")"
        Apl term' term'' -> "Apl (" ++ show term' ++ ") (" ++ show term'' ++ ")"
        Lit lit -> show lit
        Let name term' term'' -> "Let (" ++ show name ++ ") (" ++ show term' ++ ") (" ++ show term'' ++ ")"
        Func _ -> "Function"

data Literal = Int_ Int
             | Bool_ Bool
             deriving (Show, Eq)

data Function = Plus
              | Minus

int :: Term -> Int
int term = case term of
    Lit (Int_ x) -> x
    _ -> error "Type error: Expected Int"

bool :: Term -> Bool
bool term = case term of
    Lit (Bool_ x) -> x
    _ -> error "Type error: Expected Bool"

-- When you apply a function to a term, you get a term
plus :: Term
plus = Func (\x -> Func (\y -> Lit (Int_ (int x + int y))))

minus :: Term
minus = Func (\x -> Func (\y -> Lit (Int_ (int x - int y))))

ternary :: Term
ternary = Func (\x -> Func (\y -> Func (\z -> if bool x then y else z)))

incr :: Term
incr = Func (\x -> Lit (Int_ (int x + 1)))

-- Substitute the name of a variable in a term with another name
substitute :: Name -> Name -> Term -> Term
substitute old new term = case term of
    Lam u _ | u == old || u == new -> substitute old new (alphaConvert term)
    Lam u term' -> Lam u (substitute old new term')
    Var u | u == old -> Var new
    Var u -> Var u
    Let u term' term'' -> Let u (substitute old new term') (substitute old new term'')
    Lit lit -> Lit lit
    Func f -> Func f
    Apl term' term'' -> Apl (substitute old new term') (substitute old new term'')

-- Renaming a variable to allow shadowing
alphaConvert :: Term -> Term
alphaConvert term = case term of
    Lam u term' -> Lam (u ++ "'") (substitute u (u ++ "'") term')
    _ -> term

-- Evaluate an expression
eval :: Term -> Term
eval term = -- trace ("Eval: " <> show term) $
  case term of
    Apl (Lit _) _ -> error "Type error: Cannot apply a literal"
    Apl term' term'' -> apply term' term''
    Let x term' term'' -> eval $ expandLet x term' term''
    term' -> term'

-- Apply a term to another term, beta reducing if necessary
apply :: Term -> Term -> Term
apply term term' = case eval term of
    Func f -> eval $ f (eval term')
    Let x t t' -> eval $ Let x t (apply t' term')
    Lam v t -> eval $ Let v (eval term') t
    Var _ -> error $ "Type error: (apply) Cannot apply a variable. Term: " ++ show term ++ " Term': " ++ show term'
    Lit _ -> error "Type error: (apply) Cannot apply a literal"
    Apl _ _ -> error "Compile error: (apply) Cannot pass Apl into apply"


-- Let x be val_x in output
expandLet :: Name -> Term -> Term -> Term
expandLet x val_x output = case output of
    Var u | u == x -> val_x
    Var u -> Var u
    Lam u out | u == x -> expandLet x val_x (alphaConvert (Lam u out))
    Lam u term -> Lam u (expandLet x val_x term)
    Let u _ _ | u == x -> error $ "Shadowing not allowed in Let. Var: " ++ x
    Let u term term' -> Let u (expandLet x val_x term) (expandLet x val_x term')
    Apl term term' -> Apl (expandLet x val_x term) (expandLet x val_x term')
    Lit lit -> Lit lit
    Func f -> Func f

identity :: Term
identity = Lam "x" (Var "x")

ten :: Term
ten = Lit (Int_ 10)
