module DataTypes where

newtype VarId	 	= VarId Int
newtype Domain    	= Domain [Int]
type Oper	 		= (Int->Int->Int)
type Equ     		= (Int->Int->Bool)
type Heuristic 		= ([Variable]->(Variable,[Variable]))
data Variable 		= Variable String Domain
data VariableValue 	= VariableValue String Int
valOf :: VariableValue -> Int
valOf (VariableValue _ val) = val

data Expr = VI String
	| Term Int
	| Form Expr Oper Expr
data Constraint  = Constraint Expr Equ Expr
data Orr  = Orr Constraint Constraint

class NameFind a where
	nameOf :: a -> String
instance NameFind Variable where
	nameOf (Variable nam _) = nam
instance NameFind VariableValue where
	nameOf (VariableValue nam _) = nam
instance NameFind Constraint where
	nameOf (Constraint ex1 op ex2) = (nameOf ex1)++" oper " ++(nameOf ex2)
instance NameFind Expr where
	nameOf (VI var) = var 
	nameOf (Term num) = show num 
	nameOf (Form ex1 op ex2) = (nameOf ex1)++" oper " ++ (nameOf ex2)