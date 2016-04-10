module Solver where
import Data.Maybe (fromJust)
import LowLevel
import DataTypes

--	this root should call dBranchIt
solveIt ::  Heuristic -> [Constraint] -> [Variable] -> [Orr] -> [[VariableValue]]
solveIt heur cs vs orr
	--if there is an empty domain return nothing, as there are no solutions due to
	--	unary constraints or from arc revisions
	| unSfiable == True	= []
	--return the solution from the next level of the tree
	| unSfiable == False= dBranchIt heur [] newCons heurPick orr
	where
	(newCons, ncVars)	= nodesConsistent cs vs
	queue				= createQueue ncVars
	--applies AC3 to it
	acVars				= arcsConsistent queue newCons vs
	--checks for empty domains
	unSfiable			= emptyDomains acVars
	--finds the new variable to branch on
	heurPick			= heur acVars

--Reduces the domains of variables for all unary constraints then returns a
--	list with these adjusted domains and a list of constraints which are not unary
nodesConsistent :: [Constraint] -> [Variable] -> ([Constraint],[Variable])
nodesConsistent [] vs 	= ([],vs)
nodesConsistent  (con:cs) vs
	--if the constraint being checked is not unary keep the constraint in the result
	| isUnaryConst == False	= ((con:resCon), resVar)
	| otherwise				= nodesConsistent cs newVars
	where
	varNames		= varsInConst con
	isUnaryConst	= ((length varNames)==1)
	(resCon, resVar)= nodesConsistent cs vs
	newVars			= nodeConsistent (varNames!!0) con vs

nodeConsistent :: String -> Constraint -> [Variable] -> [Variable]
nodeConsistent conVar con [] = []
nodeConsistent conVar con (var : vs)
	| nam == conVar	= (reducedVar:(nodeConsistent conVar con vs))
	| otherwise		= (var:(nodeConsistent conVar con vs))
	where
	Variable nam (Domain (d:dom)) 	= var
	reducedVar						=(Variable nam (Domain (reduceUnaryDom var con)))

reduceUnaryDom :: Variable -> Constraint -> [Int]
reduceUnaryDom (Variable nam (Domain [])) _ = []
reduceUnaryDom var con
	| canBeSat	= (d : (reduceUnaryDom var con))
	| otherwise	= (reduceUnaryDom (Variable nam (Domain dom)) con)
	where
	Variable nam (Domain (d:dom)) = var
	satisfied = (evCon [(VariableValue nam d)] con)
	canBeSat	= (satisfied == Nothing) || (satisfied == Just True)

--After assigning a variable this applies arconsistensy 
--	    	Heuristic	current assignments	cnsts to sat    vars n doms   ORcnst   Possible succesful var allocation
checkBranch' :: Heuristic -> [VariableValue] -> [Constraint] -> [Variable] -> [Orr] -> [[VariableValue]]
checkBranch' heur vv cs vs orr
	--if there is an empty domain return nothing, as it is not a solution
	| unSfiable == True	= []
	--return the solution from the next level of the tree
	| unSfiable == False= dBranchIt heur vv cs heurPick orr
	where
	(mostRecent : vvs)	= vv
	--gets arcs where last changed variable is source
	queue 				= getArcsForVar (nameOf mostRecent) vs 
	--applies AC3 to it
	newVs				= arcsConsistent queue cs vs
	--checks for empy domains
	unSfiable			= emptyDomains newVs
	--finds the new variable to branch on
	heurPick			= heur newVs

--creates d branches 
--			Heuristic		vars so far			All cnsts 	(branch Var, all others)			solution
dBranchIt :: Heuristic -> [VariableValue] -> [Constraint] -> (Variable,[Variable]) -> [Orr] -> [[VariableValue]]
dBranchIt heur _ _ ((Variable _ (Domain [])),_) _ = []
dBranchIt heur vv cons (varToAssign,vars) orr
	= solution ++ (dBranchIt heur vv cons ((Variable nam (Domain (dom))),vars) orr)
	where
	--take the first value of the domain
	(Variable nam (Domain (q:dom))) = varToAssign
	solution	=  checkBranch' heur ((VariableValue nam q):vv) cons vars orr


arcsConsistent :: [(String, String)] -> [Constraint] -> [Variable] -> [Variable]
arcsConsistent [] cs vs = vs
arcsConsistent ((src,dst):arcs) cs vs
	| numOfCons > 0 = arcsConsistent arcs cs (replaceVar vs redDomsrcV)
	| otherwise		= arcsConsistent arcs cs vs
	where
	consToCheck = getConstraintsFor [] (src,dst) cs  
	numOfCons	= length consToCheck 
	redDomsrcV	= (reduceArcDom (srcVar,dstVar) consToCheck)
	srcVar		= getVar vs src 
	dstVar		= getVar vs dst
	newArcs		= addArcIfReduced (src,dst) vs arcs srcVar redDomsrcV

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
	isPoss = (existsDestSatisfy dstVar (VariableValue nam d) con)


--Recursively check that for an assigned source variable there exists a possible
--	destination value in the domain which together satisfy a single constraint
existsDestSatisfy :: Variable -> VariableValue -> Constraint -> Bool
existsDestSatisfy (Variable _ (Domain [])) _ _ = False
existsDestSatisfy (Variable dNam (Domain (q:dom))) v con
	| canBeSat	= True
	| otherwise	= existsDestSatisfy (Variable dNam (Domain (dom))) v con
	where
	satisfied	= evCon [(VariableValue dNam q),v] con
	canBeSat	= (satisfied == Nothing) || (satisfied == Just True)

--Evaluates all constraints
--	If any constraint fails return false
--	If any constraint is yet to be satisfied return Nothing
--	If all are satisfied return True
evAllCon :: [VariableValue] -> [Constraint] -> Maybe Bool
evAllCon vv []		= Just True
evAllCon vv (c:cs)
	| currConSat == Just False 	= Just False
	| allOtherCons == Just False= Just False
	| otherwise				 	= currConSat
	where
	currConSat = evCon vv c
	allOtherCons = evAllCon vv cs

--Evaluates a constraint
evCon :: [VariableValue] -> Constraint -> Maybe Bool
evCon vv (Constraint ex1 eqOp ex2)
	| ((val1 == Nothing) || (val2 == Nothing)) = Nothing
	| otherwise				   = Just (eqOp  (fromJust val1) (fromJust val2))
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
