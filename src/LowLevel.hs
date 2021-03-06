module LowLevel where
import DataTypes
import Data.Maybe (fromJust)


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

--gets all the variables of a constraint and checks if there is only one
varsInConst :: Constraint -> [String]
varsInConst (Constraint ex1 op ex2) = (varsInExpr ex1) ++ (varsInExpr ex2) 

--finds out if an expression contains a variable
varsInExpr :: Expr -> [String]
varsInExpr (Term t)		= []
varsInExpr (VI name)		= [name]
varsInExpr (Form ex1 op ex2)	= varsInExpr ex1 ++ varsInExpr ex2

emptyDomains :: [Variable] -> Bool
--last element of list
emptyDomains [(Variable n (Domain dom))]
	--there is an empty domain with in the current variable
	| dom == []		= True
	--got all the way through without finding an empty domain
	| otherwise		= False
emptyDomains ((Variable n (Domain dom)):vs) 
	--there is an empty domain with in the current variable
	| dom == []		=  True
	--no empty domain, keep checking
	| otherwise		= emptyDomains vs

--wrapper to make code more understandable in main solver
createQueue :: [Variable] -> [(String,String)]
createQueue vs = createQueue' vs vs

-- Creates arcs for all variable to all other variables
--				all vars 	need to make arcs For 	arcs made
createQueue' :: [Variable] -> [Variable] -> [(String,String)]
createQueue' _ [] = []
createQueue' vs ((Variable nam (Domain dom)):vars) =
	let 
		arcsForCurr = getArcsForVar nam vs
	in arcsForCurr ++ createQueue' vs vars 

--creates arcs for one variable with this input as the source, ignoring an arc to self
--				varName 	all vars 		arcs made
getArcsForVar :: String -> [Variable] -> [(String,String)]
getArcsForVar _ [] = []
getArcsForVar vNam ((Variable nam (Domain dom)):vars)
	| dstIsSelf	= getArcsForVar vNam vars
	| otherwise	= ((nam,vNam):(getArcsForVar vNam vars))
	where
	dstIsSelf = (vNam==nam)

--					arc which caused	all vars	current arcs 			old var 	new var 		new arcs
addArcIfReduced :: (String,String) -> [Variable] -> [(String, String)] -> Variable -> Variable -> [(String, String)]
addArcIfReduced (src,dst) vars ls (Variable oldNam (Domain oldDom)) (Variable newNam (Domain newDom))
	| isUnchanged	= ls
	| otherwise		= ls ++ newArcs
	where
	isUnchanged = ((length newDom)==(length oldDom))
	--creates arcs without the arc from dst to src
	newArcs = removeArc (getArcsForVar newNam vars) (dst,src) 

--Takes in a list of arcs and removes one arc
--				all arcs 			unwanted one		output arcs
removeArc :: [(String,String)] -> (String,String) -> [(String,String)]
removeArc [] _ = []
removeArc ((src,dst):arcs) (unWantSrc,unWantDst)
	| isUnWanted= removeArc arcs (unWantSrc,unWantDst)
	| otherwise	= ((src,dst):(removeArc arcs (unWantSrc,unWantDst)))
	where
	isUnWanted = ((src == unWantSrc) &&(dst == unWantDst))

--From the list of assigned variable finds the value, if not there returns Nothing
getVarVal :: [VariableValue] -> String -> Maybe Int
getVarVal [] s = Nothing
getVarVal (v:vs) s
	| s == nameOf v	= Just (valOf v)
	| otherwise	= getVarVal vs s

--From the list of assigned variable finds the value, if not there returns Nothing
getVar :: [Variable] -> String -> Variable
getVar [] s = error ("Searched for non-defined variable: "++s)
getVar (v:vs) s
	| s == nameOf v	= v
	| otherwise	= getVar vs s

--From the list of assigned variable finds the value, if not there returns Nothing
replaceVar :: [Variable] -> Variable -> [Variable]
replaceVar [] repV = error ("Tried to replace non-defined variable: " ++ nameOf repV)
replaceVar (v:vs) repV
	| nameOf repV == nameOf v	= repV:vs
	| otherwise					= (v:(replaceVar vs repV))

removeVar :: [Variable] -> String -> [Variable]
removeVar [] remNam = error ("Tried to replace non-defined variable: " ++ remNam)
removeVar (v:vs) remNam
	| remNam == nameOf v	= vs
	| otherwise				= (v:(removeVar vs remNam))

--return constraints with these input variables in them
--					soFarFound		vars2Find			allConst		Constrains with vars
getConstraintsFor :: [Constraint] -> (String, String) -> [Constraint] -> [Constraint]
getConstraintsFor conCons (src, dst) [] = conCons
getConstraintsFor conCons (src, dst) (c:cs)
	| hasBoth	= getConstraintsFor (c:conCons) (src,dst) cs
	| otherwise	= getConstraintsFor conCons (src,dst) cs
	where
	Constraint ex1 op ex2 = c
	hasBoth = ((exprContainsVar src ex1) && (exprContainsVar dst ex2)) || ((exprContainsVar dst ex1) && (exprContainsVar src ex2)) || ((exprContainsVar src ex1) && (exprContainsVar dst ex1)) || ((exprContainsVar src ex2) && (exprContainsVar dst ex2))

--From a list of domains, gets the variables required
--					So far   	  varName	input vars    outvars
getRelatedVarDom :: [Variable] -> String -> [Variable] -> [Variable]
getRelatedVarDom sofar nam [] = []
getRelatedVarDom sofar nam (v:vs)
	| vNam == nam	= getRelatedVarDom (v:sofar) nam vs
	| otherwise		= getRelatedVarDom sofar nam vs
	where
	Variable vNam vDom = v

--finds out if an expression contains a variable
exprContainsVar :: String -> Expr -> Bool
exprContainsVar v (Term t)		= False
exprContainsVar v (VI name)
	| v == name = True
	| otherwise =  False
exprContainsVar v (Form ex1 op ex2)	= (exprContainsVar v ex1) || (exprContainsVar v ex2)
