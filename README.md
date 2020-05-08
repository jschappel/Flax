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

<statement>     ::= <ifStmt>
                 | <whileStmt>
                 | <exprStmt>
                 | <block>
                 | <break>
                 | <returnStmt>

<returnStmt>     | "return" expression? ";"

<break>         ::= "break" ";"

<ifStmt>        ::= "if" <expression> <block> ( "else" <block> )?

<whileStmt>     ::= "while" <expression> <block>

<block>         ::= "{" declaration* "}"

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

### Native Functions
- print     => prints a value on a line
- println   => prints a value on a new line
- clock     => returns the current unix time 


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
- [X] Add functions to the language
- [X] Add basic native functions to the language
- [ ] Add anonymous functions to the language
- [ ] Make the language turing complete
- [ ] Add immutable variables to the language
- [ ] Add Structures to the language

### Syntax

- Basic math
    ```javascript
    1 + 2;
    13 * 32;
    13 / 12;
    4 - 1;
    ```

- logic operators:
    ```javascript
    true and true   // true
    false or true   // true 
    !true           // false 
    ```

- Equality: 
    ```javascript
    "1" == 1        // true
    "2" != "3"      // true
    4 <= 5          // true
    4 >= 5          // false
    5 > 7           // false
    5 < 7           // true 
    ```


- string concatenation
    ```haskell
    "Foo" ++ "Bar";
    "Foo" ++ 1; 
    ```

- declare a variable: 
    ```javascript
    let x = 10;
    ```
- assignment:
    ```javascript
    let x = 70;
    x = 2;
    ```

- Scoping: 
    ```javascript
    let x = 10;
    let y = 20;
    {
        x = 20;
        let y = 56; //shadowing
    }
    ```

- conditionals: 
    ```rust
    if x < 10 {
        return true;
    }

    if x > 10 {
        return false;
    } else {
        return true;
    }
    ```

- ternary operator:
    ```javascript
    5 > 6 ? true : false
    ```

- print to console:
    ```rust 
    print("Hello");     // Prints hello
    println("Hello")    // Prints hello on a new line
    ```

- incrementing:
    ```rust
    let i = 10;
    i += 2;      // i is now 12
    i -= 1;      // i is now 11

- looping:
    ```rust
    let x = 10;
    while x < 20 {
        println(x);
        x+=1;
    }
    ```

- functions: 
    ```rust
    func factorial(x) {
        if x < 2 {
            return 1;
        }
        return factorial(x-1);
    }

    let ans = factorial(5);
    ```