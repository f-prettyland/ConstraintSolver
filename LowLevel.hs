module LowLevel where
import DataTypes

--finds out if an expression contains a variable
exprContainsVar :: String -> Expr -> Bool
exprContainsVar v (Term t)		= False
exprContainsVar v (VI name)
	| v == name = True
	| otherwise = False
exprContainsVar v (Form ex1 op ex2)	= (exprContainsVar v ex1) || (exprContainsVar v ex2)

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

--return constraints with these input variables in them
--					soFarFound		vars2Find			allConst		Constrains with vars
getConstraintsFor :: [Constraint] -> (String, String) -> [Constraint] -> [Constraint]
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