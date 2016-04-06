import System.Environment ( getArgs )
import Text.Regex.Posix
import Data.Text

--a variable and what it is bonded by
data Constraint = Constraint Int [Int] [(Com, Int)]

data LongConstraint = LongConstraint String (Maybe [Int]) (Maybe (Com, String))

data Com = Com (a -> a -> Bool)

solve :: [(Int,[Int],[Int])] -> [Int]

branch :: [Int] -> [(Int,[In)] -> [Int]


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

--todo: parse for .. vs , for making range vs individual values
--gets the domain that a variable can span from a string
domGet :: String -> [Int]
domGet dom
	| dom =~ ","  = nonConsecDomGet (spliceOn "," dom)
	| otherwise   = [low..high]
	where
	split = spliceOn ".." dom
:	low = read (split!!0)
	high = read (split!!1)

--builds the domain of possible numbers
nonConsecDomGet :: [String] -> [Int]
nonConsecDomGet [] = []
nonConsecDomGet (s:ss) = ((read s)::Int : nonConsecDomGet ss )


--gets the constraint from a line
constLine :: String -> LongConstraint
constLine line = split!!0 Nothing (op, split!!1)
	where
	split = spliceOn " " line
	op = getOp split!!1

--takes in a string and finds the operator for it
getOp :: String -> Com
getOp sym
	| sym == "==" = (==)
	| sym == "/=" = (/=) -- :/
	| sym == ">"  = (>) 
	| sym == "<"  = (<)
	| sym == "<=" = (<=) -- :)
	| sym == ">=" = (>=) -- :(


--splits up a string from a given string value
spliceOn :: String -> String -> [String]
spliceOn sp big =  Prelude.map unpack (splitOn (pack sp) (pack big))

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
