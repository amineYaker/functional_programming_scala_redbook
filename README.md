# functional_programming_scala_redbook
# chapter1: benefits of fp
- referential transparency => local reasoning
- composability => cleaner code

# chapter2: basics
- tail rec (functional loops)
- higher order functions
- polymorphic functions (polymorphic arguments)
- currying

# chapter3: ADT, pattern matching
- purely functional data structures (List, Tree)
- writing and generalizing pure functions
- pattern matching mechanics
- basic mechanics (fold, map, filter, flatMap)

# chapter4: Error handling

- Option & Either mechanisms
- reasoning about code from the function signature
# chapter5: lazy evaluation
- functional streams === pull based
- non-strictness => separating description from evaluation
- memoizing with lazy vals (caching)
- laziness makes code reusable and optimized without having to manually short cut the loops
- => B (taking a parameter by name) which make a function choose if it wants to evaluate it or not 

# chapter6: state
- using pure functions that accepts a state as an argument and returns the new state with the result
- for comprehensions make it look like imperative programming
- flatMap with the help of unit can generate both map and map2
- basic State Monad functions (modify, set , get)

# chapter7: Par
- functional library (how to abstract functions).
- API as an Algebra.
- how to rewrite and abstract the code.

# chapter8: Property based testing lib
- abstract algebra vs concrete implementation
- convenient functions
- reuse of same patterns (map, flatMap ...)

# chapter9: Parser
- algebraic design
- thinking in terms of library and function capabilities before low level implementation