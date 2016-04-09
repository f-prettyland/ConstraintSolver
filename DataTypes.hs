module DataTypes where

class NameFind a where
	nameOf :: a -> String

instance NameFind Variable where
	nameOf (Variable nam _) = nam

instance NameFind VariableValue where
	nameOf (VariableValue nam _) = nam

newtype VarId	 = VarId Int
newtype Domain    = Domain [Int]
type Oper	 = (Int->Int->Int)
type Equ     = (Int->Int->Bool)

data Variable 		= Variable String Domain
--nameOf :: Variable -> String
--nameOf (Variable nam _) = nam

data VariableValue = VariableValue String Int
--nameOf :: VariableValue -> String
--nameOf (VariableValue nam _) = nam

valOf :: VariableValue -> Int
valOf (VariableValue _ val) = val

data Expr = VI String
	| Term Int
	| Form Expr Oper Expr
data Constraint  = Constraint Expr Equ Expr
data Orr  = Orr Constraint Constraint