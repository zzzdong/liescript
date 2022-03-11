Grammar
===========


file: statement*


## Statements

Statement :
     ;
    | Item
    | LetStatement
    | ExpressionStatement

### Items

Item : 
     ;
    | Use
    | TypeAlias
    | ConstantItem
    | StaticItem
    | Function
    | Struct
    | Enum
    | Trait
    | Implementation
    | Mod

#### Use

```rust
use std;
use std::*;
use std::path;
use std::path as p;
use std::{path, json};
use std::{path as p, json as j};
```


Use : 
     `use` UseTree `;`

UseTree:
     SimplePath
    | SimplePath (`::` `*`)?
    | SimplePath `::` `{` UseTree `}`
    | SimplePath (`as` Ident)

SimplePath :
     PathSeg (`::` PathSeg)?

PathSeg :
     Ident
    | `crate`
    | `super`
    | `self`



## Expression

Express :
     LiteralExpression
    | PathExpression
    | OperatorExpression
    | GroupedExpression
    | ArrayExpression
    | IndexExpression
    | CallExpression
    | MethodCallExpression
    | FieldAccessExpression

## Top Level

```




```