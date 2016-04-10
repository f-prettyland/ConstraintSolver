module Heuristics where
import DataTypes

staticOrder :: [Variable]->(Variable,[Variable])
staticOrder (v:vs) = (v, vs)