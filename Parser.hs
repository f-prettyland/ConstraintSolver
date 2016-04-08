
import Solver
import Helpful
import System.Environment ( getArgs )
import	Text.Regex.Posix
import	Data.List 	( elemIndex )

possEqualities = ["==", "!=", "<", ">", "<=", ">="]


--creates subtype for parsing 
parseLines :: [String] -> ([Constraint],[Variable])
parseLines ss = parseLines' [] [] ss


--todo: extend this to array defn
parseLines' :: [Constraint] -> [Variable] -> [String] -> ([Constraint],[Variable])
parseLines' cs vs [] = (cs,vs)
parseLines' cs vs (s:ss)
	| True == isDom  = ((cs ++ cons ), (vs ++ ( domLine s : vars )))
	| False == isDom = ((cs ++ ( constLine s : cons )), ( vs ++ vars ))
	where
	isDom = s =~ "Let"
	(cons,vars) = parseLines ss

--gets the constraint from a line
constLine :: String -> Constraint
constLine line = Constraint ex1 op ex2
	where
	(opChar, split) = getWhatSplicedOn possEqualities line
	op = getOp opChar
	ex1 = makeExpr (split!!0)
	ex2 = makeExpr (split!!1)

makeExpr :: String -> Expr
makeExpr p = VI p

--gets the domain and variable name for a line
domLine :: String -> Variable
domLine line = Variable (split!!1) dom
	where
	split = spliceOn " " line
	dom = domGet (split!!4)

--gets the domain that a variable can span from a string
domGet :: String -> Domain
domGet dom
	| dom =~ ","  = Domain (nonConsecDomGet (spliceOn "," dom))
	| otherwise   = Domain [low..high]
	where
	split = spliceOn ".." dom
	low = read (split!!0)
	high = read (split!!1)

--builds the domain of possible numbers
nonConsecDomGet :: [String] -> [Int]
nonConsecDomGet [] = []
nonConsecDomGet (s:ss) = (((read s)::Int) : nonConsecDomGet ss )


--takes in a string and finds the operator for it
getOp :: String -> Equ
getOp sym
	| sym == "==" = (==)
	| sym == "!=" = (/=) -- /:
	| sym == ">"  = (>) 
	| sym == "<"  = (<)
	| sym == "<=" = (<=) -- (:
	| sym == ">=" = (>=) -- ):


main = do
	args <- getArgs
	constraintFile <- readFile (args !! 0)
	let cnstLines = Prelude.lines constraintFile
	print ("g")