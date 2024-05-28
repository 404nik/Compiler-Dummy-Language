# COL226 Assignment 3

Welcome to the repository for COL226 Assignment 3. This assignment focuses on context-free grammar and the implementation of abstract syntax trees (AST) in SML.

## Context-Free Grammar

In this assignment, we define a context-free grammar for a programming language. Below are the terminal symbols and non-terminal symbols defined in the grammar:

### Terminal Symbols
- PROG | DCOL | VAR | COL | SCOL | INT | BOOL | COMMA | CURL | CURR | IS | READ | WRITE | IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWH | ROUNDL | ROUNDR | OR | BY | DT | DAND | DF | EXCLAMATION | LESSER | LESSEREQ | EQUALop | GREATERop | GREATEREQ | TRIANGLEBR | ADD | SUBTRACT | MULTIPLY | DIVIDE | MOD | ID of string | NUM of int | EOF

### Non-Terminal Symbols (AST Nodes)
- Program of AST.pro
- Block of AST.blo
- DeclarationSeq of AST.decS
- Declaration of AST.dec
- Type of AST.typ
- VariableList of AST.varL
- CommandSeq of AST.comS
- Command of AST.com
- Expression of AST.exp
- IntExpression of AST.expI
- IntTerm of AST.ter
- IntFactor of AST.fac
- BoolExpression of AST.expB
- BoolTerm of AST.terB
- BoolFactor of AST.facB
- Comparison of AST.comp
- Variable of AST.var
- RelOp of AST.rel
- AddOp of AST.add
- MultOp of AST.mul
- Identifier of AST.ide
- Numeral of AST.num
- Dummy1 of AST.dum
- Dummy2 of AST.dumm

### Context-Free Grammar

The context-free grammar is defined based on the above symbols and includes production rules for generating valid program constructs.

## AST Datatype Definition

In this section, we define the abstract syntax tree (AST) data types in SML. Each non-terminal symbol corresponds to a specific data type, and the terminal symbols are represented accordingly.

## Compilation Instructions

To compile the program, follow these steps in the terminal:

1. Open SML by typing `sml` and pressing enter.
2. Type `CM.make "while_ast.cm";` and press enter.

## Acknowledgements

The implementation of this assignment relies on the following files provided by Arun Kumar Sir:
- com.sml
- g.sml
- while_ast.sml

## Usage

To use the provided grammar and AST implementation, refer to the appropriate files and functions for parsing and generating abstract syntax trees.

---

For any questions or issues, please contact the course instructors or teaching assistants. Happy coding!
