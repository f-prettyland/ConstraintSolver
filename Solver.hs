import System.Environment ( getArgs )

main = do
	args <- getArgs
	constraintFile <- readFile (args !! 0)
	let cnstLines = lines constraintFile
	print (cnstLines!!2)
