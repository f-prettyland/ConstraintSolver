import Data.Maybe (fromJust)
import Data.Typeable (typeOf)
--		 id
newtype VarId	 = VarId Int
--type Oper	 = (Expr -> Eq Int -> Expr -> Int)

data Domain    = Domain [Int]

data Expr = VI VarId
	| Term Int
	| Formula Expr (Int -> Int ->Bool) Expr

data Constraint  = Constraint Expr (Expr->Expr->Bool) Expr


maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

evCon ::  Constraint -> Bool
evCon (Constraint ex1 op ex2) = (op ex1 ex2)

evEx :: Expr -> Int
evEx (Term t) 			= t
evEx (Formula ex1 op ex2)	= evFrm(ex1 op ex2)

evFrm :: Expr -> (Int->Int->Bool) -> Expr -> Bool
evFrm  ex1 op ex2 = op (rl1 rl2) --todo change this to lookupVar
	where
	rl1 = evEx ex1
	rl2 = evEx ex2
--singleton :: a -> Tree a
--singleton x = Node x EmptyTree EmptyTree

--tr :: Tree -> Int
--tr EmptyTree = 3

--treeInsert :: (Ord a) => a -> Tree a -> Tree a
--treeInsert x EmptyTree = singleton x

main = do
	print(fromJust(maybeRead "12"::Maybe Int))
