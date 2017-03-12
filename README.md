# Constraint Solver
A (verbosely written) haskell constraint solver with d-way branching and AC-3.  

## How  
1. Create a `.cnst` file (see `./models/` for examples)  
2. Chuck definitions in  
3. Run `make`, then `./Solver` on it  

###Variables  
Define variables like this:  
```
Let x be in 0..7
Let y be in 0..3
```
Where `x` and `y` are your appropriately named variables, and `0..7` represents the domain allowing for values from 0 to 7 inclusive.  
###Constraints  
Define variables like this:  
```
a != c-1
```
Where `a` and `c` are possible your appropriately named variables, and between then you can use the relations `==`, `!=`, `>`, `<`, `<=`, `>=` and operations `*`, `+`, `-`.  
###Heuristics  
At the beginning of your file you can use `Heuristic ` followed by `sdf` for smallest domain first, or `static` for normal execution.  

## Todo  
- [ ] Parsing arrays  
- [ ] Parsing loops  
- [ ] More heuristics  
- [ ] Parallelisation  
