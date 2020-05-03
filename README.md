### Core Grammar
```shell

<program>       ::= <declaration>* EOF

<declaration>   ::= <varDecl>
                 | <statement>

<varDecl>       ::= "var" IDENTIFIER ( "=" <expression> )? ";"

<statement>     ::= <printStmt>
                 | <exprStmt>

<printStmt>     ::= "print" expression ";"

<exprStmt>      ::= expression ";"

<expression>    ::= <binary>
                 | <unary>
                 | <grouping>
                 | <literal>

<binary>        ::= <expression> <operator> <expression>

<unary>         ::= ("-" | "!") <expression>

<literal>       ::= NUMBER | STRING | "true" | "false" | "nil"

<grouping>      ::= "(" <expression> ")"

<operator>      ::= "==" | "++" | "+" | "-" | "*" | "/" |
                   ">=" | "<=" | ">" | "<"

```

#### Expanded Expression Grammar
```shell
<expression>        ::= <assignment>

<assignment>        ::= IDENTIFIER "=" assignment
                     | <equality>

<equality>          ::= <comparison> ( ( '==' | '!=' ) <comparison> )*

<comparison>        ::= <addition> ( ('>' | '<' | '>=' '<=' ) <addition> )*

<addition>          ::= <multiplication> ( ( '+' | '-' ) <multiplication> )*

<multiplication>    ::= <unary> ( ( '*' | '/' ) <unary> )*

<unary>             ::= ( '-' | '!' ) <unary>
                     | <primary>

<literal>           ::= NUMBER | STRING | true | false | nil
                     | "(" <expression> ")" 
                     | IDENTIFIER
```



### Design Choices:
- Flax follows Ruby's design where all value besides ```false``` and ```nil``` are true
- Only Integers can compared using ```>```, ```<```, ```>=```, ```<=```
- Flax uses ```++``` to concatenate strings just like Haskell
