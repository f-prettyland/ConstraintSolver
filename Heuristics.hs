module Heuristics where
import DataTypes

staticOrder :: [Variable]-> (Variable,[Variable])
staticOrder (v:vs)	=(v, vs)

sdf :: [Variable]-> (Variable,[Variable])
sdf (v:vs)	= sdf' [] v vs

sdf' :: [Variable] -> Variable -> [Variable]-> (Variable,[Variable])
sdf' others smallest [] = (smallest,others)
sdf' others smallest (newVar:vs)
 	| isSmaller	= sdf' (others++[smallest]) newVar vs
 	| otherwise	= sdf' (others++[newVar]) smallest vs
 	where
	(Variable _ (Domain smallDom))	= smallest
 	(Variable _ (Domain nDom))		= newVar
 	isSmaller 						= ((length smallDom)>(length nDom))