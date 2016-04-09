module Solver where
import Data.Maybe (fromJust)
import LowLevel
import DataTypes


--todo separate out the unary constraints propigate them before input here
--solveIt :: ([Constraint],[Variable],[Orr]) -> Maybe [VariableValue]

--	    current assignment  cnsts to sat    vars n doms   ORcnst   Possible succesful var allocation
--solveIt' :: [VariableValue] -> [Constraint] -> [Variable] -> [Orr] -> Maybe [VariableValue]
--solveIt' vv cs vs or un
--	| sfd == Just True	= vv
--	| sfd == Just False	= Nothing
--	| sfd == Nothing	= 
--	where
--	--checks if is is satified or satifiable
--	satfied = 


--todo, deal with unary
--Finds out if the constraints are still satifiable with 
--nodeConsistent :: [VariableValue] -> [Constraint] -> [Orr] -> Maybe Bool


arcsConsistent :: [(String, String)] -> [Constraint] -> [Variable] -> [Variable]
arcsConsistent [] cs vs = vs
arcsConsistent ((src,dst):arcs) cs vs
	| numOfCons > 0 = arcsConsistent arcs cs (replaceVar vs reducedDomvs)
	| otherwise		= arcsConsistent arcs cs vs
	where
	consToCheck = getConstraintsFor [] (src,dst) cs  
	numOfCons	= length consToCheck 
	reducedDomvs= (reduceArcDom (srcVar,dstVar) consToCheck)
	srcVar		= getVar vs src 
	dstVar		= getVar vs dst

--Takes in an arc and reduces the domain
reduceArcDom :: (Variable, Variable) -> [Constraint] -> Variable
reduceArcDom (src,dst) [] = src
reduceArcDom (src,dst) (con:cs) =
	let redSrc = Variable (nameOf src) (Domain (getValidSourceDom (src,dst) con))
		in reduceArcDom (redSrc, dst) cs

--Recursively goes over all the values of the domain ensuring each can have a
--	satisfied constraint, with at least one destination value
getValidSourceDom :: (Variable,Variable) -> Constraint -> [Int]
getValidSourceDom ((Variable nam (Domain [])),_) _ = []
getValidSourceDom (srcVar,dstVar) con
	--calls this function on the remaining domain but appends the successful value to
	--	the front
	| isPoss 	= (d : (getValidSourceDom (srcVar,dstVar) con))
	--creates a new variable with the element just tested not within the domain
	--	and then calls itself upon this
	| otherwise	= (getValidSourceDom ((Variable nam (Domain dom)),dstVar) con)
	where
	--breaking down the source of the arc's variable into it's name and domain parts
	Variable nam (Domain (d:dom)) = srcVar
	--testing the value taken from the top of the domain against all values of the 
	--	source domain
	isPoss = existsDestSatisfy dstVar (VariableValue nam d) con


--Recursively check that for an assigned source variable there exists a possible
--	destination value in the domain which together satisfy a single constraint
existsDestSatisfy :: Variable -> VariableValue -> Constraint -> Bool
existsDestSatisfy (Variable _ (Domain [])) _ _ = False
existsDestSatisfy (Variable dNam (Domain (q:dom))) v con
	| satisfied 		= True
	| otherwise			= existsDestSatisfy (Variable dNam (Domain (dom))) v con
	where
	satisfied = evCon [(VariableValue dNam q),v] con

--Evaluates a constraint
evCon :: [VariableValue] -> Constraint -> Bool
evCon vv (Constraint ex1 eqOp ex2)
	| ((val1 == Nothing) || (val2 == Nothing)) = True
	| otherwise				   = eqOp  (fromJust val1) (fromJust val2)
	where
	val1 = (evEx vv ex1) 
	val2 = (evEx vv ex2)

--Evaluates an expression, possibly returning nothing
evEx :: [VariableValue] -> Expr -> Maybe Int
evEx vv (Term t) 		= (Just t)
evEx vv (VI name)		= (getVarVal vv name) 
evEx vv (Form ex1 op ex2)	= (evFrm vv ex1 op ex2)

--Evaluates a formula where a 
evFrm :: [VariableValue] -> Expr -> Oper -> Expr -> Maybe Int
evFrm vv ex1 op ex2
	| ((val1 == Nothing) || (val2 == Nothing)) = Nothing
	| otherwise				   = Just (op (fromJust val1) (fromJust val2))
	where
	val1 = evEx vv ex1
	val2 = evEx vv ex2
