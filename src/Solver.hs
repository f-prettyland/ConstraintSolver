module Solver where
import LowLevel
import DataTypes
import ArcConsistent

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


