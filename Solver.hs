import System.Environment ( getArgs )
import Text.Regex.Posix
import Data.Text

parseLines :: [String] -> [(String, [Int])]
parseLines (s:ss) = ( varDomLine s : parseLines ss )

varDomLine :: String -> (String, [Int])
varDomLine line = (split!!0, dom)
	where
	split = spliceOn " " line
	dom = domGet (split!!2)

domGet :: String -> [Int]
domGet dom = [low..high]
	where
	split = spliceOn "," dom
	low = read (split!!0)
	high = read (split!!1)

spliceOn :: String -> String -> [String]
spliceOn sp big =  Prelude.map unpack (splitOn (pack sp) (pack big))


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
