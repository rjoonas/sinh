module Declare where

import Data.Maybe (fromJust)
import Prelude hiding (LT, GT, EQ)
import Test.QuickCheck

data BinaryOp = Add | Sub | Mult | Div | Power | And | Or | GT | LT | LE | GE | EQ
  deriving (Show, Eq, Enum)

level :: BinaryOp -> Int
level Or = 1
level And = 2
level GT = 3
level LT = 3
level LE = 3
level GE = 3
level EQ = 3
level Add = 4
level Sub = 4
level Mult = 5
level Div = 5
level Power = 6

name :: BinaryOp -> String
name Or = "||"
name And = "&&"
name GT = ">"
name LT = "<"
name LE = "<="
name GE = ">="
name EQ = "=="
name Add = "+"
name Sub = "-"
name Mult = "*"
name Div = "/"
name Power = "^"

data UnaryOp = Neg | Not deriving (Show,Eq)

data Value = IntV Int | BoolV Bool deriving (Eq)

data Type = TInt | TBool deriving (Eq)

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"

data Program = Program FunEnv Exp

type FunEnv = [(String, Function)]

data Function = Function [(String, Type)] Exp Type

data Exp = Lit Value
         | Unary UnaryOp Exp
         | Bin BinaryOp Exp Exp
         | If Exp Exp Exp
         | Var String
         | Decl String Exp Exp
         | Call String [Exp]

prog1 :: Program
prog1 = Program fenv exp where
  fenv = [
    ("absolute", Function [("x",TInt)] (If (Bin GT (Var "x") (Lit (IntV 0))) (Var "x") (Unary Neg (Var "x"))) TInt),
    ("max", Function [("x",TInt),("y",TInt)] (If (Bin GT (Var "x") (Var "y")) (Var "x") (Var "y")) TInt)]
  exp = Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)]

-- Question 4: show program
instance Show Program where
  show (Program fenv exp) = showSep "\n" $ map showFun fenv ++ [show exp]

prop_showProg1 :: Bool
prop_showProg1 = show prog1 == showSep "\n"
  ["function absolute(x : Int) : Int {",
   " if (x > 0) x; else -x",
   "}",
   "function max(x : Int, y : Int) : Int {",
   " if (x > y) x; else y",
   "}",
   "max(absolute(-5), 4)"]

{-
Question 1: concat a list of strings with a separator.

NOTE: We could just write showSep = Data.List.intercalate but I'm wary of using library functions because in the FP course we often get point reductions for that... So here's my own implementation of intercalate for good measure:
-}
separatorCat :: [a] -> [[a]] -> [a]
separatorCat sep (x:xs) = foldl (\a b -> a ++ sep ++ b) x xs
separatorCat sep [] = []

showSep :: String -> [String] -> String
showSep = separatorCat

prop_showSep1 :: Bool
prop_showSep2 :: Bool
prop_showSep3 :: Bool
prop_showSep1 = showSep "; " ["hello", "to", "world"] == "hello; to; world"
prop_showSep2 = showSep "; " ["hello"] == "hello"
prop_showSep3 = showSep "; " [] == ""

showArg :: (String, Type) -> String
showArg (name, t) = name ++ " : " ++ show t

-- Question 3: show for function definitions.
showFun :: (String, Function) -> String
showFun (name, Function args body returnType) = showSep "\n" [
  "function " ++ name ++ "(" ++ showSep ", " (map showArg args) ++ ") : " ++ show returnType ++ " {",
  " " ++ show body,
  "}"]

showFunExample :: Function
showFunExample = Function [("x",TInt), ("y",TInt)] (Bin Add (Var "x") (Var "y")) TInt

prop_showFun1 :: Bool
prop_showFun1 = showFun ("foo", showFunExample) == "function foo(x : Int, y : Int) : Int {\n x + y\n}" 

instance Show Exp where show = showExp 0

prop_showCall1 :: Bool
prop_showCall1 = show (Call "max" [Lit (IntV 3), Lit (IntV 4)]) == "max(3, 4)"

prop_showCall2 :: Bool
prop_showCall2 = show (Call "abs" [Lit (IntV 3)]) == "abs(3)"

showExp :: Int -> Exp -> String
showExp _ (Lit i) = show i
showExp _ (Var x) = x
showExp _ (Unary Neg a) = "-" ++ showExp 99 a
showExp _ (Unary Not a) = "!" ++ showExp 99 a
showExp lev (Bin op a b) = showBin lev (level op) a (name op) b

showExp level (Decl x a b) =
  let result = "var " ++ x ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
  in if level > 0 then paren result else result

showExp level (If a b c) =
  let result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
  in if level > 0 then paren result else result

-- Question 2: show function for function calls.
showExp _ (Call f args) = f ++ "(" ++ showSep ", " (map (showExp 0) args)  ++ ")"

paren :: String -> String
paren x = "(" ++ x ++ ")"

showBin :: Int -> Int -> Exp -> String -> Exp -> String
showBin outer inner a op b =
  let result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b in
  if inner < outer then paren result else result
