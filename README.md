# Flax

### Why am I doing this?
I have always been fascinated with programing languages and the different advantages of each one. I decided that I would like to build my own language in order to better understand how they are implemented. Flax is a general purpose, C inspired language that draws from both functional and Object Oriented design principles. Some of the languages that Flax draws inspiration from are:
- Racket
- Haskell
- Java
- C
- Rust

### Core Grammar
```shell

<program>       ::= <declaration>* EOF

<declaration>   ::= <funDecl>
                 | <varDecl>
                 | <statement>

<funDecl>       ::= "fn" <function>

<function>      ::= IDENTIFIER "(" parameters? ")" <block>

<parameters>    ::= IDENTIFIER ( "," IDENTIFIER )*

<varDecl>       ::= "var" IDENTIFIER ( "=" <expression> )? ";"

<statement>     ::= <printStmt>
                 | <ifStmt>
                 | <whileStmt>
                 | <exprStmt>
                 | <block>
                 | <break>

<break>         ::= "break" ";"

<ifStmt>        ::= "if" <expression> <block> ( "else" <block> )?

<whileStmt>     ::= "while" <expression> <block>

<block>         ::= "{" declaration* "}"

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
                     | <logical_or>
                     | <increment>

<increment>         ::= IDENTIFIER "+=" <addition>
                     | IDENTIFIER "-=" <addition>

<logical_or>        ::= logical_and ( "or" logical_and )*

<logical_and>       ::= equality ( "and" equality )*

<conditional>       ::= <equality> ( "?" <expression> ":" <conditional> )?

<equality>          ::= <comparison> ( ( '==' | '!=' ) <comparison> )*

<comparison>        ::= <addition> ( ('>' | '<' | '>=' '<=' ) <addition> )*

<addition>          ::= <multiplication> ( ( '+' | '-' ) <multiplication> )*

<multiplication>    ::= <unary> ( ( '*' | '/' ) <unary> )*

<unary>             ::= ( '-' | '!' ) <unary>
                     | <call>

<call>              ::= <literal> ( "(" arguments? ")" )*

<arguments>         ::= <expression> ( "," expression )*

<literal>           ::= NUMBER | STRING | true | false | nil
                     | "(" <expression> ")" 
                     | IDENTIFIER
```



### Design Choices:
- Flax follows Ruby's design where all value besides ```false``` and ```nil``` are true
- Only Integers can compared using ```>```, ```<```, ```>=```, ```<=```
- Flax uses ```++``` to concatenate strings just like Haskell
- Flax uses ```let``` to create a variable. Shadowing is allowed
- Flax uses ```and``` and ```or``` for logical operators


### Road Map
- [X] Add basic addition and multiplication
- [X] Add statements to the language 
- [X] Update the Repl to read .flax files
- [X] Add global assignment to the language
- [X] Add lexical scoping to the language
- [X] Add conditionals to the language
- [X] Add loops to the language
- [X] Add increments (+= -=)
- [ ] Add functions to the language
- [ ] Make the language turing complete
- [ ] Add immutable variables to the language
- [ ] Add Structures to the language