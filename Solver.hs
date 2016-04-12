module Solver where
import Data.Maybe (fromJust)
import LowLevel
import DataTypes

--	this root should call dBranchIt
solveIt ::  Heuristic -> [Constraint] -> [Variable] -> [Orr] -> [[VariableValue]]
solveIt heur cs [] orr = []
solveIt heur cs vs orr
	--if there is an empty domain return nothing, as there are no solutions due to
	--	unary constraints or from arc revisions
	| unSfiable == True = []
	--return the solution from the next level of the tree
	| unSfiable == False=   dBranchIt heur [] newCons heurPick orr
	where
	(newCons, ncVars)	= nodesConsistent cs vs
	queue				= createQueue ncVars
	--applies AC3 to it
	acVars				= arcsConsistent queue newCons ncVars
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
	newVars			=  nodeConsistent (varNames!!0) con vs

nodeConsistent :: String -> Constraint -> [Variable] -> [Variable]
nodeConsistent conVar con [] = []
nodeConsistent conVar con (var : vs)
	| nam == conVar	= (reducedVar:(nodeConsistent conVar con vs))
	| otherwise		= (var:(nodeConsistent conVar con vs))
	where
	Variable nam (Domain (d:dom)) 	= var
	reducedDom						= reduceUnaryDom var con
	reducedVar						=(Variable nam (Domain reducedDom))

reduceUnaryDom :: Variable -> Constraint -> [Int]
--reduceUnaryDom (Variable nam (Domain [])) _ = []
reduceUnaryDom var con
	| fullDom==[]			= []
	| canBeSat	 			= (d : (reduceUnaryDom (Variable nam (Domain dom)) con))
	| otherwise				= (reduceUnaryDom (Variable nam (Domain dom)) con)
	where
	Variable nam (Domain (fullDom)) = var
	(d:dom) 	= fullDom
	satisfied 	= (evCon [(VariableValue nam d)] con)
	canBeSat	= (satisfied == Nothing) || (satisfied == Just True)

--After assigning a variable this applies arconsistensy 
--	    	Heuristic	current assignments	cnsts to sat    vars n doms   ORcnst   Possible succesful var allocation
checkBranch' :: Heuristic -> [VariableValue] -> [Constraint] -> [Variable] -> [Orr] -> [[VariableValue]]
checkBranch' heur vv cs [] orr = [vv]
checkBranch' heur vv cs vs orr
	--if there is an empty domain return nothing, as it is not a solution
	| unSfiable == True	= []
	--return the solution from the next level of the tree
	| unSfiable == False= dBranchIt heur vv cs heurPick orr
	where
	(mostRecent : vvs)	= vv
	--gets arcs where last changed variable is source
	queue				= getArcsForVar (nameOf mostRecent) vs 
	--applies AC3 to it
	newVs				= forwadProp queue mostRecent cs vs
	--checks for empy domains
	unSfiable			= emptyDomains newVs
	--finds the new variable to branch on
	heurPick			= heur newVs
	--todo remove:
	(p,pars)			=heurPick

--creates d branches 
--			Heuristic		vars so far			All cnsts 	(branch Var, all others)			solution
dBranchIt :: Heuristic -> [VariableValue] -> [Constraint] -> (Variable,[Variable]) -> [Orr] -> [[VariableValue]]
--dBranchIt heur _ _ ((Variable _ (Domain [])),_) _ = trace ("finished branch ") $ []
dBranchIt heur vv cons (varToAssign,vars) orr
	| fullDom==[]= []
	| otherwise	 = solution ++ ((dBranchIt heur vv cons ((Variable nam (Domain (dom))),vars) orr))
	where
	isEmpty		= emptyDomains [varToAssign]
	--take the first value of the domain
	Variable nam (Domain (fullDom)) = varToAssign
	(q:dom) 						= fullDom
	solution	= checkBranch' heur ((VariableValue nam q):vv) cons vars orr

forwadProp :: [(String,String)] -> VariableValue -> [Constraint] -> [Variable] -> [Variable]
forwadProp [] vv cs vs =   vs
forwadProp ((src,dst):arcs) vv cs vs
	| numOfCons > 0 = forwadProp arcs vv cs (replaceVar vs redDomsrcV)
	| otherwise		= forwadProp arcs vv cs vs
	where
	consToCheck = getConstraintsFor [] (src,dst) cs  
	numOfCons	= length consToCheck 
	redDomsrcV	= (evaluateArcConstraints (srcVar) vv consToCheck)
	srcVar		= getVar vs src
	newArcs		= addArcIfReduced (src,dst) vs arcs srcVar redDomsrcV

--Takes in an arc and reduces the domain
evaluateArcConstraints :: Variable -> VariableValue  -> [Constraint] -> Variable
evaluateArcConstraints src vv [] = src
evaluateArcConstraints src vv (con:cs) =
	let redSrc = Variable (nameOf src) (Domain (checkPossible src vv con))
		in evaluateArcConstraints redSrc vv cs

checkPossible :: Variable -> VariableValue -> Constraint -> [Int]
checkPossible srcVar vv con
	| isEmpty	= []
	| canBeSat 	= (d : (checkPossible nextIter vv con))
	| otherwise	= (checkPossible nextIter vv con)
	where
	isEmpty		= emptyDomains [srcVar]
	Variable nam (Domain (d:dom)) = srcVar
	nextIter	= (Variable nam (Domain dom))
	satisfied	= (evCon [(VariableValue nam d),vv] con)
	canBeSat	= (satisfied == Nothing) || (satisfied == Just True)


arcsConsistent :: [(String, String)] -> [Constraint] -> [Variable] -> [Variable]
arcsConsistent [] cs vs =  vs
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
	| isEmpty	= []
	--calls this function on the remaining domain but appends the successful value to
	--	the front
	| isPoss 	= (d : (getValidSourceDom nextIter con))
	--creates a new variable with the element just tested not within the domain
	--	and then calls itself upon this
	| otherwise	= (getValidSourceDom nextIter con)
	where
	isEmpty		= emptyDomains [srcVar]
	--breaking down the source of the arc's variable into it's name and domain parts
	Variable nam (Domain (d:dom)) = srcVar
	nextIter	=((Variable nam (Domain dom)),dstVar)
	--testing the value taken from the top of the domain against all values of the 
	--	source domain
	isPoss = (existsDestSatisfy dstVar (VariableValue nam d) con)


--Recursively check that for an assigned source variable there exists a possible
--	destination value in the domain which together satisfy a single constraint
existsDestSatisfy :: Variable -> VariableValue -> Constraint -> Bool
--existsDestSatisfy (Variable _ (Domain [])) _ _ = False
existsDestSatisfy var v con
	| isEmpty	= False
	| canBeSat	= True
	| otherwise	= existsDestSatisfy (Variable dNam (Domain (dom))) v con
	where
	isEmpty		= emptyDomains [var]
	(Variable dNam (Domain (q:dom))) = var
	--satisfied	= trace("attempting "++(show q)++ " for "++dNam++" domain size "++ (show (length dom)))$ evCon [(VariableValue dNam q),v] con
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
--trace ("not goonaa ") $
	| unSat =  Nothing
	--trace ((show (nameOf (vv!!0)))++(show (nameOf (vv!!1)))++(show conRes)) $
	| otherwise	=  Just conRes
	where
	val1 = (evEx vv ex1) 
	val2 = (evEx vv ex2)
	conRes= (eqOp  (fromJust val1) (fromJust val2))
	unSat= ((val1 == Nothing) || (val2 == Nothing))

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
