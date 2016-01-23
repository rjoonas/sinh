module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe
import Test.QuickCheck

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary :: BinaryOp -> Value -> Value -> Value
binary Add   (IntV a)  (IntV b)  = IntV (a + b)
binary Sub   (IntV a)  (IntV b)  = IntV (a - b)
binary Mult  (IntV a)  (IntV b)  = IntV (a * b)
binary Div   (IntV a)  (IntV b)  = IntV (a `div` b)
binary Power (IntV a)  (IntV b)  = IntV (a ^ b)
binary And   (BoolV a) (BoolV b) = BoolV (a && b)
binary Or    (BoolV a) (BoolV b) = BoolV (a || b)
binary LT    (IntV a)  (IntV b)  = BoolV (a < b)
binary LE    (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE    (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT    (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ    a         b         = BoolV (a == b)

type Binding = (String, Value)
type Env = [Binding]

execute :: Program -> Value
execute (Program funEnv main) = evaluate main [] funEnv

evaluate :: Exp -> Env -> FunEnv -> Value
evaluate e en fenv = eval e en where
  eval :: Exp -> Env -> Value
  eval (Lit n) _ = n
  eval (Var v) env = fromMaybe (error "Not found") (lookup v env)
  eval (Unary op ex) env = unary op (eval ex env)
  eval (Bin op e1 e2) env = binary op (eval e1 env) (eval e2 env)
  eval (Decl v exp body) env = eval body ((v, eval exp env) : env)

  eval (If cond trueBranch falseBranch) env =
    let BoolV c = eval cond env in
    if c then eval trueBranch env else eval falseBranch env

  -- Question 5: Evaluation for function calls.
  eval (Call fun args) env =
    let Function xs body returnType = fromJust $ lookup fun fenv
        newEnv = zip (map fst xs) [eval a env | a <- args]
    in eval body newEnv

prop_execute :: Bool
prop_execute = execute prog1 == IntV 5
