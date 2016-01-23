module TypeCheck where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe
import Test.QuickCheck

type TEnv = [(String,Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt = Just TInt
tunary Not TBool = Just TBool
tunary _ _ = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add TInt  TInt  = Just TInt
tbinary Sub TInt  TInt  = Just TInt
tbinary Mult TInt TInt  = Just TInt
tbinary Div TInt  TInt  = Just TInt
tbinary And TBool TBool = Just TBool
tbinary Or  TBool TBool = Just TBool
tbinary LT  TInt  TInt  = Just TBool
tbinary LE  TInt  TInt  = Just TBool
tbinary GE  TInt  TInt  = Just TBool
tbinary GT  TInt  TInt  = Just TBool
tbinary EQ  t1    t2    | t1 == t2 = Just TBool
tbinary _ _ _ = Nothing

-- Question 6: checkFunEnv1
checkFunEnv :: FunEnv -> Maybe TFunEnv
checkFunEnv fds = checkFunEnv1 fds [] -- starts with an empty function type environment
  where checkFunEnv1 :: FunEnv -> TFunEnv -> Maybe TFunEnv
        checkFunEnv1 [] fenv = Just fenv
        checkFunEnv1 ((name, Function params body returnType) : fs) fenv = 
          let recursionEnv = (name, (params, returnType)) : fenv in
          tcheck body params recursionEnv >>= \t -> checkFunEnv1 fs ((name, (params, t)) : fenv)

tcheck :: Exp -> TEnv -> TFunEnv -> Maybe Type
-- Question 7: type checking for function calls.
tcheck (Call name args) tenv fenv = do
  (params, t) <- lookup name fenv
  argtypes <- mapM (\e -> tcheck e tenv fenv) args
  if map snd params == argtypes then Just t else Nothing 

tcheck (Lit v) _ _ = Just $ case v of
  IntV _ -> TInt
  BoolV _ -> TBool

tcheck (Unary op e) tenv fenv = tcheck e tenv fenv >>= tunary op

tcheck (Bin op e1 e2) tenv fenv = do
  t1 <- tcheck e1 tenv fenv
  t2 <- tcheck e2 tenv fenv
  tbinary op t1 t2

tcheck (If cond trueBranch falseBranch) tenv fenv = do
  tcheck cond tenv fenv
  trueType <- tcheck trueBranch tenv fenv
  falseType <- tcheck falseBranch tenv fenv
  if trueType == falseType then Just trueType else Nothing

tcheck (Var v) tenv _ = lookup v tenv

tcheck (Decl v e1 e2) tenv fenv = tcheck e1 tenv fenv >>= \t -> tcheck e2 ((v,t) : tenv) fenv

-- Question 8: type checking at the program level.
checkProgram :: Program -> Bool
checkProgram (Program fds main) = isJust $ checkFunEnv fds >>= tcheck main []

prop_checkProg1 :: Bool
prop_checkProg1 = checkProgram prog1
