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
data Orr  = Orr Constraint Constraint

--todo separate out the unary constraints propigate them before input here
solveIt :: ([Constraint],[Variable],[Orr]) -> Maybe [VariableValue]

--	    current assignment  cnsts to sat    vars n doms   ORcnst   Possible succesful var allocation
solveIt' :: [VariableValues] -> [Constraint] -> [Variable] -> [Orr] -> Maybe [VariableValue]
solveIt' cs vs or un
	| sfd == Just True	= 
	| sfd == Just False	= Nothing
	| sfd == Nothing	= 
	where
	--checks if is is satified or satifiable
	satfied = 



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
