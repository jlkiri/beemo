# Beemo grammar

```
declaration := func

func := identifier ('(' params? ')')? ':' block

statement := return | print | condition | while
return := 'return' expr
print := 'print' expr
condition := 'if' expr ':' block ('else' ':' block)?

expr := assignment
assignment := logical ('->' identifier)?
logical := equality (('or' | 'and') equality)*
equality := comparison ('==' comparison)*
comparison := term (('>' | '<' | '>=' | '<=') term)*
term := factor (('+' | '-') factor)*
factor := unary (('*' | '/') unary)*
unary := ('!' | '-') unary | call-like
call-like := primary '[' term ']' | call
call := primary ('(' arguments? ')')*
primary := identifier | 'true' | 'false' | number ('>>' identifier) | array | '(' expr ')'
```

### Utility rules

```
block := indent statement* dedent
params := identifier (',' identifier)*
arguments := expr (',' expr)*
array := '{' members? '}'
members := number (',' number)*
indent := '\t'
```
