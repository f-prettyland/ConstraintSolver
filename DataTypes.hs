module DataTypes where

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