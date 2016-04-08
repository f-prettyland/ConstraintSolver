
import Solver
import Helpful
import System.Environment ( getArgs )
import	Text.Regex.Posix
import	Data.List 	( elemIndex )

data StringConstraint  = StringConstraint String Equ String

--gets the constraint from a line
--constLine :: String -> Constraint
--constLine line = split!!0 Nothing (op, split!!1)
--	where
--	split = spliceOn " " line
--	op = getOp split!!1

--gets the domain and variable name for a line
domLine :: String -> (String, Domain)
domLine line = (split!!0, dom)
	where
	split = spliceOn " " line
	dom = domGet (split!!2)

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
	| sym == "/=" = (/=) -- :/
	| sym == ">"  = (>) 
	| sym == "<"  = (<)
	| sym == "<=" = (<=) -- :)
	| sym == ">=" = (>=) -- :(


main = do
	args <- getArgs
	constraintFile <- readFile (args !! 0)
	let cnstLines = Prelude.lines constraintFile
	print ("g")