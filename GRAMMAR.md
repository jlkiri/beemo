```
declaration := func

func := identifier ('(' params? ')')? ':' block

statement := return | print
return := 'return' expr

expr := equality (('or' | 'and') equality)*
equality := comparison ('==' comparison)*
comparison := term (('>' | '<' | '>=' | '<=') term)*
term := factor (('+' | '-') factor)*
factor := unary (('*' | '/') unary)*
unary := ('!' | '-') unary | call
call := primary ('(' arguments? ')')*
primary := identifier | 'true' | 'false' | number
```

```
block := indent statement* dedent
params := identifier (',' identifier)*
arguments := expr (',' expr)*
indent := '\t'
```

```
5.times(|| vec.push(thing))
```
