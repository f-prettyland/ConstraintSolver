import Solver
import Helpful
import Heuristics
import DataTypes
import System.Environment ( getArgs )
import Text.Regex.Posix
import Data.List 	( elemIndex )
import Data.Maybe ( fromJust )

import Debug.Trace (trace)

--todo in here check if looking for multiple solns or not
solutionsToOutput :: [[VariableValue]] -> Bool -> IO ()
solutionsToOutput [] _ = putStrLn("All solutions printed")
solutionsToOutput (s:slns) allSoln
	| allSoln	= solnToOutput s >> solutionsToOutput slns allSoln
	| otherwise	= solnToOutput s

solnToOutput :: [VariableValue] -> IO ()
solnToOutput = mapM_ (\(VariableValue na val) -> putStrLn (na++" = "++(show val))) 

--creates subtype for parsing 
parseLines :: [String] -> (Heuristic, [Constraint],[Variable])
parseLines ss = parseLines' (staticOrder) [] [] ss

--todo: extend this to array defn
parseLines' :: Heuristic -> [Constraint] -> [Variable] -> [String] -> (Heuristic,[Constraint],[Variable])
parseLines' heur cs vs [] = (heur, cs,vs)
parseLines' heur cs vs (s:ss)
	| isDom		= parseLines' heur cs (domLine s : vs) ss
	| isHeur	= parseLines' (heurLine s) cs vs ss
	| isConst	= parseLines' heur (constLine s :cs) vs ss
	| otherwise	= parseLines' heur cs vs ss
	where
	isDom	= s =~ domKey
	isHeur	= s =~ heuristicKey
	isConst	= s =~ constraintReg

--gets the constraint from a line
constLine :: String -> Constraint
constLine line =  (Constraint ex1 op ex2)
	where
	(opChar, split) = getWhatSplicedOn possEqualities line
	op = getOp opChar
	ex1 = makeExpr (split!!0)
	ex2 = makeExpr (split!!1)

--assumes only string name of thing, no addition, no constraints
makeExpr :: String -> Expr
makeExpr expres
	| isExpr			= (Form (makeExpr (split!!0)) (getCalc opChar) (makeExpr (split!!1)))
	| poss /= Nothing	= Term (fromJust poss)
	| otherwise			= VI (trim expres)
	where
	poss 	= (maybeRead (trim expres) :: Maybe Int)
	isExpr	= expres =~ operationReg
	(opChar, split) = getWhatSplicedOn possOperations expres

heurLine :: String -> Heuristic
heurLine line
      | x == "static"	= staticOrder
      | x == "sdf"		= sdf
      | otherwise		= staticOrder
      where
      split = spliceOn " " line
      x		= split!!1

--gets the domain and variable name for a line
domLine :: String -> Variable
domLine line = Variable (trim (split!!1)) dom
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

getCalc :: String -> Oper
getCalc sym
	| sym == "*" = (*)
	| sym == "+" = (+)
	| sym == "-"  = (-) 

allSolnFind :: [Char] -> Bool
allSolnFind x
      | x == "all"	= True
      | otherwise	= False

main = do	args <- getArgs
		constraintFile <- (if (length args) > 0 then readFile (args !! 0) else error "No file given")
		--allSolns <- maybeRead (args !! 1)
		--constraintFile <- (readFile "test.cnst")
		let allSolns = (if (length args) > 1 then allSolnFind (args !! 1) else False)
		let cnstLines = lines constraintFile
		let (her, pop, vars) = parseLines cnstLines
		let solns 			= solveIt her pop vars []
		solutionsToOutput solns allSolns