import	System.Environment ( getArgs )
import	Text.Regex.Posix
import 	Data.Text ( splitOn,pack, unpack )
import	Data.Maybe	( fromJust )
import	Data.List 	( elemIndex )
--a variable and what it is bonded by
--                           id  dom   [(operator, const2_id)]
data Constraint = Constraint Int [Int] [(Eq Int, Int)]

data LongConstraint = LongConstraint String (Maybe [Int]) (Maybe (Eq Int, String))

--data Com = Com (Eq a => a -> a -> Bool)

--solve :: [Constraint] -> [Int]

--branch :: [Int] -> [Constraint] -> [Int]

--changeCnsts :: [LongConstraint] -> [Constraint]

--todo check if it exists in CSs already
--from string contraints converts to the int constraints, checking only for the definitions
createDomVars' :: [String] -> [LongConstraint] -> [Constraint] -> ([String], [Constraint])
createDomVars' cnstInd [] cs    = (cs,cnstInd)
createDomVars' cnstInd (lc :lcs) cs
--createDomVars' cnstInd ((name dom con):lcs) cs
	| dom /= Nothing = (subInds, ((Constraint newInd dom [] ):subCnst))
	| otherwise	 = createDomVars' cnstInd lcs --ignores constraint
	where
	(inds, newInd) = getIndexAddIfNot name cnstInd
	(subInds, subCnst) = createDomVars' inds lcs cs
	LongConstraint name dom con   = lc
	
assignCnsts' :: [String] -> [Constraint] -> [LongConstraint] -> [Constraint]
assignCnsts' nameInd cs [] = []
assignCnsts' nameInd cs (lc:lcs)
--assignCnsts' nameInd cs ((name dom con):lcs)
	| con /= Nothing = ((defWCnst):lcsCss) 
	| otherwise	 = assignCnsts' nameInd cs lcs --ignores definition
	where
	(comp, strId) = fromJust(con)
	ind      = fromJust (elemIndex name nameInd)
	cnstInd  = fromJust(elemIndex strId nameInd)
	defWCnst = assCnst (fromJust(con)) (name dom con)
	lcsCss   = assignCnsts' nameInd cs lcs
	LongConstraint name dom con = lc

--Adds a constraint to some constraint
assCnst :: (Eq Int, Int) -> Constraint -> Constraint
assCnst c id dom cs = id dom cs++c
	
--from an id returns the constraint
getConstraint' :: Int -> [Constraint] -> Maybe Constraint
getConstraint' i []     = Nothing
getConstraint' i (c :cs)
	| i == id   = (id stuff)
	| otherwise = getConstraint' i cs
	where
	Constraint id stuff = c

--searches a list of strings for value, if it's not there, it adds it to the list, overall it returns the index
getIndexAddIfNot :: String -> [String] -> ([String], Int)
getIndexAddIfNot s inds
	| index /= Nothing = (inds, (fromJust index))
	| otherwise	   = (inds++s, (length inds))
	where
	index = elemIndex s inds

--creates subtype for parsing 
parseLines :: [String] -> [LongConstraint]
parseLines [] = []
parseLines (s:ss)
	| True == isDom  = ( domLine s : otherLines )
	| False == isDom = ( constLine  s : otherLines )
	where
	isDom = (length s) == 4
	otherLines = parseLines ss

--gets the domain and variable name for a line
domLine :: String -> LongConstraint (String, [Int])
domLine line = split!!0 dom Nothing
	where
	split = spliceOn " " line
	dom = domGet (split!!2)

--gets the domain that a variable can span from a string
domGet :: String -> [Int]
domGet dom
	| dom =~ ","  = nonConsecDomGet (spliceOn "," dom)
	| otherwise   = [low..high]
	where
	split = spliceOn ".." dom
	low = read (split!!0)
	high = read (split!!1)

--builds the domain of possible numbers
nonConsecDomGet :: [String] -> [Int]
nonConsecDomGet [] = []
nonConsecDomGet (s:ss) = (((read s)::Int) : nonConsecDomGet ss )


--gets the constraint from a line
constLine :: String -> LongConstraint
constLine line = split!!0 Nothing (op, split!!1)
	where
	split = spliceOn " " line
	op = getOp split!!1

--takes in a string and finds the operator for it
getOp :: String -> Eq Int
getOp sym
	| sym == "==" = (==)
	| sym == "/=" = (/=) -- :/
	| sym == ">"  = (>) 
	| sym == "<"  = (<)
	| sym == "<=" = (<=) -- :)
	| sym == ">=" = (>=) -- :(


--splits up a string from a given string value
spliceOn :: String -> String -> [String]
spliceOn sp big =  map unpack (splitOn (pack sp) (pack big))

--debug parse printing
out' :: [(String, [Int])] -> String
out' [] = ""
out' ((a,i):ss) = a ++ (show (i!!0)) ++ (show(i!!1))++"\n" ++ out' ss

main = do
	args <- getArgs
	constraintFile <- readFile (args !! 0)
	let cnstLines = Prelude.lines constraintFile
	print (out' (parseLines cnstLines))
--	print parseLines
--	print (cnstLines!!1 =~~ "(y)" :: Maybe String)
