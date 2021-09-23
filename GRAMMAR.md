```
decl := func
func := identifier '(' params? ')' ':' block

params := identifier (',' identifier)*
block := indent expr dedent
```

```
5.times(|| vec.push(thing))
```
