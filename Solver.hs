module Solver where

newtype VarId	 = VarId Int
newtype Domain    = Domain [Int]
type Oper	 = (Int->Int->Int)
type Equ     = (Int->Int->Bool)

data Variable 		= Variable String Domain
data VariableValues = VariableValue String Int
data Expr = VI String
	| Term Int
	| Form Expr Oper Expr
data Constraint  = Constraint Expr Equ Expr

evCon :: [VariableValues] -> Constraint -> Bool
evCon vv (Constraint ex1 eqOp ex2) = eqOp  (evEx vv ex1) (evEx vv ex2)

evEx :: [VariableValues] -> Expr -> Int
evEx vv (Term t) 			= t
evEx vv (Form ex1 op ex2)	= evald
	where
	evald = evFrm vv ex1 op ex2

evFrm :: [VariableValues] -> Expr -> Oper -> Expr -> Int
evFrm vv ex1 op ex2 = op rl1 rl2
	where
	rl1 = evEx vv ex1
	rl2 = evEx vv ex2
