module Solver where
import Data.Maybe (fromJust)

newtype VarId	 = VarId Int
newtype Domain    = Domain [Int]
type Oper	 = (Int->Int->Int)
type Equ     = (Int->Int->Bool)

data Variable 		= Variable String Domain
data VariableValue = VariableValue String Int
nameOf :: VariableValue -> String
nameOf (VariableValue nam _) = nam

valOf :: VariableValue -> Int
valOf (VariableValue _ val) = val

data Expr = VI String
	| Term Int
	| Form Expr Oper Expr
data Constraint  = Constraint Expr Equ Expr
data Orr  = Orr Constraint Constraint

--todo separate out the unary constraints propigate them before input here
solveIt :: ([Constraint],[Variable],[Orr]) -> Maybe [VariableValue]

--	    current assignment  cnsts to sat    vars n doms   ORcnst   Possible succesful var allocation
solveIt' :: [VariableValues] -> [Constraint] -> [Variable] -> [Orr] -> Maybe [VariableValue]
solveIt' vv cs vs or un
	| sfd == Just True	= vv
	| sfd == Just False	= Nothing
	| sfd == Nothing	= 
	where
	--checks if is is satified or satifiable
	satfied = 

arcsConsistent :: [(String, String)] -> [Constraint] -> [Variable] -> [Variable]
arcsConsistent [] cs vs = vs
arcsConsistent ((src,dest):arcs) cs vs
	| numOfCons > 0 = 
	| otherwise		= arcsConsistent arcs cs vs
	where
	consToCheck = getConstraintsFor [] (src,dest) cs  
	numOfCons	= length consToCheck 

arcConsistent :: (String, String) -> [Constraint] -> [Variable] -> [Variable]
arcConsistent (src,dst) [] vs = vs
arcConsistent (src,dst) (c:cs) vs
	| evCon 
	where
	/\[vv]/\ = ()

--return constraints with these input variables in them
--					soFarFound		vars2Find			allConst		Constrains with vars 
getConstraintsFor :: [Constraint] -> (String, String) -> [Constraints] -> [Constraint]
getConstraintsFor conCons (src, dst) (c:cs)
	| hasBoth	= getConstraintsFor (c:conCons) (src,dst) cs
	| otherwise	= getConstraintsFor conCons (src,dst) cs
	where
	Constraint ex1 op ex2 = c
	hasBoth = ((exprContainsVar src ex1) && (exprContainsVar dst ex2)) || ((exprContainsVar dst ex1) && (exprContainsVar src ex2)) || ((exprContainsVar src ex1) && (exprContainsVar dst ex1)) || ((exprContainsVar src ex2) && (exprContainsVar dst ex2))


exprContainsVar :: String -> Expr -> Bool
exprContainsVar v (Term t)		= False
exprContainsVar v (VI name)
	| v == name = True
	| otherwise = False
exprContainsVar v (Form ex1 op ex2)	= (exprContainsVar ex1) || (exprContainsVar ex2)

--todo, deal with unary
--Finds out if the constraints are still satifiable with 
--nodeConsistent :: [VariableValues] -> [Constraint] -> [Orr] -> Maybe Bool

getVarVal :: [VariableValue] -> String -> Maybe Int
getVarVal [] s = Nothing
getVarVal (v:vs) s
	| s == nameOf v	= Just (valOf v)
	| otherwise	= getVarVal vs s

evCon :: [VariableValue] -> Constraint -> Bool
evCon vv (Constraint ex1 eqOp ex2)
	| ((val1 == Nothing) || (val2 == Nothing)) = True
	| otherwise				   = eqOp  (fromJust val1) (fromJust val2)
	where
	val1 = (evEx vv ex1) 
	val2 = (evEx vv ex2)

evEx :: [VariableValue] -> Expr -> Maybe Int
evEx vv (Term t) 		= (Just t)
evEx vv (VI name)		= (getVarVal vv name) 
evEx vv (Form ex1 op ex2)	= (evFrm vv ex1 op ex2)

evFrm :: [VariableValue] -> Expr -> Oper -> Expr -> Maybe Int
evFrm vv ex1 op ex2
	| ((val1 == Nothing) || (val2 == Nothing)) = Nothing
	| otherwise				   = Just (op (fromJust val1) (fromJust val2))
	where
	val1 = evEx vv ex1
	val2 = evEx vv ex2
