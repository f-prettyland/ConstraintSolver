module Helpful where
import Data.Char (isSpace)
import Data.Text ( splitOn, pack, unpack )
import Data.Maybe (fromJust)


-------
--STRING HANDLING
-------
--todo removethis comment:  usage" fromJust(maybeRead "12"::Maybe Int) "
maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

--trims off whitespace
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

--splits up a string from a given string value
spliceOn :: String -> String -> [String]
spliceOn sp big =  map unpack (splitOn (pack sp) (pack big))

getWhatSplicedOn :: [String] -> String -> (String,[String])
getWhatSplicedOn [] big = ("", []) --error in constraint parsing
getWhatSplicedOn (sp:sps) big
	| sucSplit == 1	= getWhatSplicedOn sps big
	| otherwise		= (sp, res)
	where
	res = spliceOn sp big
	sucSplit = length res