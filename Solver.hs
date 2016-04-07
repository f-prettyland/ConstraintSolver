module Solver(
  evCon, Constraint, Domain, Equ
)where

import Data.Typeable (typeOf)
newtype VarId	 = VarId Int
newtype Domain    = Domain [Int]

type Oper	 = (Int->Int->Int)
type Equ     = (Int->Int->Bool)


data Expr = VI VarId
	| Term Int
	| Form Expr Oper Expr

data Constraint  = Constraint Expr Equ Expr

evCon ::  Constraint -> Bool
evCon (Constraint ex1 eqOp ex2) = eqOp  (evEx ex1) (evEx ex2)

evEx :: Expr -> Int
evEx (Term t) 			= t
evEx (Form ex1 op ex2)	= evald
	where
	evald = evFrm ex1 op ex2

evFrm :: Expr -> Oper -> Expr -> Int
evFrm  ex1 op ex2 = b --todo change this to lookupVar
	where
	rl1 = evEx ex1
	rl2 = evEx ex2
	b   = op rl1 rl2
