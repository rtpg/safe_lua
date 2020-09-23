Todos:
  - Factor out the sourcemaps
  
List kinds:
  - a "Lua list" is an actual table
  - a "return list" is the result of doing `return 1, 2, 3` , also the value of `...`
  - a "parameter list" is what's passed into a function. `...` gets expanded on calls
    (So `f(1, ...)` will expand `...` into n parameters) 
