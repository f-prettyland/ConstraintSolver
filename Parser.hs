
import Solver
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import Data.Text ( splitOn, pack, unpack )
import System.Environment ( getArgs )
import	Text.Regex.Posix
import	Data.List 	( elemIndex )


--todo removethis comment:  usage" fromJust(maybeRead "12"::Maybe Int) "
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

--takes in a string and finds the operator for it
getOp :: String -> Equ
getOp sym
	| sym == "==" = (==)
	| sym == "/=" = (/=) -- :/
	| sym == ">"  = (>) 
	| sym == "<"  = (<)
	| sym == "<=" = (<=) -- :)
	| sym == ">=" = (>=) -- :(


--gets the constraint from a line
constLine :: String -> Constraint
constLine line = split!!0 Nothing (op, split!!1)
	where
	split = spliceOn " " line
	op = getOp split!!1

--gets the domain and variable name for a line
domLine :: String -> (String, Domain)
domLine line = split!!0 dom
	where
	split = spliceOn " " line
	dom = domGet (split!!2)

--gets the domain that a variable can span from a string
domGet :: String -> Domain
domGet dom
	| dom =~ ","  =  nonConsecDomGet (spliceOn "," dom)
	| otherwise   =  [low..high]
	where
	split = spliceOn ".." dom
	low = read (split!!0)
	high = read (split!!1)

--builds the domain of possible numbers
nonConsecDomGet :: [String] -> [Int]
nonConsecDomGet [] = []
nonConsecDomGet (s:ss) = (((read s)::Int) : nonConsecDomGet ss )

-------
--STRING HANDLING
-------

--trims off whitespace
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

--splits up a string from a given string value
spliceOn :: String -> String -> [String]
spliceOn sp big =  map unpack (splitOn (pack sp) (pack big))

main = do
	args <- getArgs
	constraintFile <- readFile (args !! 0)
	let cnstLines = Prelude.lines constraintFile
	print ("g")