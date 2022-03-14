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
use std::fs;
use std::fs as f;
use std::{fs, encoding};
use std::{path as p, json as j};
use std::{net::TcpStream as TcpSocket, encoding::json};
use crate::parser;

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