module Solver where
import Data.Maybe (fromJust)
import LowLevel


--todo separate out the unary constraints propigate them before input here
solveIt :: ([Constraint],[Variable],[Orr]) -> Maybe [VariableValue]

--	    current assignment  cnsts to sat    vars n doms   ORcnst   Possible succesful var allocation
solveIt' :: [VariableValue] -> [Constraint] -> [Variable] -> [Orr] -> Maybe [VariableValue]
solveIt' vv cs vs or un
	| sfd == Just True	= vv
	| sfd == Just False	= Nothing
	| sfd == Nothing	= 
	where
	--checks if is is satified or satifiable
	satfied = 


--todo, deal with unary
--Finds out if the constraints are still satifiable with 
--nodeConsistent :: [VariableValue] -> [Constraint] -> [Orr] -> Maybe Bool


arcsConsistent :: [(String, String)] -> [Constraint] -> [Variable] -> [Variable]
arcsConsistent [] cs vs = vs
arcsConsistent ((src,dest):arcs) cs vs
	| numOfCons > 0 = arcsConsistent arcs cs reducedDomvs
	| otherwise		= arcsConsistent arcs cs vs
	where
	consToCheck = getConstraintsFor [] (src,dest) cs  
	numOfCons	= length consToCheck 
	reducedDomvs= (reduceArcDom (src,dest) consToCheck vs)

--Takes in an arc and reduces the domain
--reduceArcDom :: (String, String) -> [Constraint] -> [Variable] -> [Variable]
--reduceArcDom (src,dst) [] vs = vs
--reduceArcDom (src,dst) (c:cs) vs
--	| evCon 
--	where
--	/\[vv]/\ = ()

--Recursively goes over all the values of the domain ensuring each can have a
--	satisfied constraint, with at least one destination value
getValidSourceDom :: (Variable,Variable) -> Constraint -> Domain
getValidSourceDom ((Variable nam (Domain [])),_) _ = Domain []
getValidSourceDom (srcVar,dstVar) con
	| isPoss == True= Domain ((d : (getValidSourceDom (srcVar,dstVar) con)))
	| otherwise		= Domain (getValidSourceDom ((Variable nam dom),dstVar) con)
	where
	isPoss = pop
	Variable nam (Domain (d:dom)) = srcVar

--Recursively check that for an assigned source variable there exists a possible
--	destination value in the domain which together satisfy a single constraint
existsDestSatisfy :: Variable -> VariableValue -> Constraint -> Bool
existsDestSatisfy (Variable _ []) _ _ = False
existsDestSatisfy (Variable dNam (Domain (q:dom))) v con
	| satisfied = True	= True
	| otherwise			= existsDestSatisfy (Variable dNam (Domain (dom))) v con
	where
	satisfied = evCon ((VariableValue dNam q):v) con

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
