import Solver
import Helpful
import Heuristics
import DataTypes
import System.Environment ( getArgs )
import Text.Regex.Posix
import Data.List 	( elemIndex )
import Data.Maybe ( fromJust )

possEqualities = ["==", "!=", "<", ">", "<=", ">="]

solutionsToOutput :: [[VariableValue]] -> IO ()
solutionsToOutput (s:slns) = solnToOutput s

solnToOutput :: [VariableValue] -> IO ()
solnToOutput = mapM_ (\(VariableValue na val) -> putStrLn (na++" = "++(show val))) 

--creates subtype for parsing 
parseLines :: [String] -> (Heuristic, [Constraint],[Variable])
parseLines ss = parseLines' (staticOrder) [] [] ss

--todo: extend this to array defn
parseLines' :: Heuristic -> [Constraint] -> [Variable] -> [String] -> (Heuristic,[Constraint],[Variable])
parseLines' heur cs vs [] = (heur, cs,vs)
parseLines' heur cs vs (s:ss)
	| True == isDom		= parseLines' heur cs (domLine s : vs) ss
	| True == isHeur	= parseLines' (heurLine s) cs vs ss
	| False == isDom	= parseLines' heur (constLine s :cs) vs ss
	where
	isDom				= s =~ "Let"
	isHeur				= s =~ "Heuristic"

--gets the constraint from a line
constLine :: String -> Constraint
constLine line = Constraint ex1 op ex2
	where
	(opChar, split) = getWhatSplicedOn possEqualities line
	op = getOp opChar
	ex1 = makeExpr (split!!0)
	ex2 = makeExpr (split!!1)

--assumes only string name of thing, no addition, no constraints
makeExpr :: String -> Expr
makeExpr expres
	| poss /= Nothing = Term (fromJust poss)
	| otherwise = VI expres
	where
	poss = maybeRead expres

heurLine :: String -> Heuristic
heurLine line
      | x == "static"	= staticOrder
      | otherwise		= staticOrder
      where
      split = spliceOn " " line
      x		= split!!1

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

main = do	args <- getArgs
		constraintFile <- readFile (args !! 0)
		let cnstLines = lines constraintFile
		let (her, pop, vars) = parseLines cnstLines
		let solns 			= solveIt her pop vars []
		solutionsToOutput solns

--todo: removes
		--let (VariableValue op val)	= (solns!!0)!!0
		--let (Variable op dom)= vars!!0
		--print (cnstLines!!2)
--		let (heur, cons, vs) = 
		
		--print(op)
--		