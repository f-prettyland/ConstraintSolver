module ArcConsistent where
import DataTypes
import LowLevel

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
