open AST
%%
(* required declarations *)
%name WHILE

%term PROG  | DCOL | VAR | COL | SCOL 
| INT | BOOL  | COMMA  | CURL  | CURR  | IS | READ | WRITE | IF 
| THEN  | ELSE  | ENDIF  | WHILE  | DO  | ENDWH 
| ROUNDL  | ROUNDR  | OR  | BY  | DT  | DAND  | DF 
| EXCLAMATION| LESSER| LESSEREQ  | EQUALop | GREATERop  | GREATEREQ  | TRIANGLEBR  | ADD 
| SUBTRACT  | MULTIPLY | DIVIDE | MOD | ID of string  | NUM of int | EOF 

%nonterm Program of AST.pro 
| Block of AST.blo 
| DeclarationSeq of AST.decS 
| Declaration of AST.dec 
| Type of AST.typ 
| VariableList of AST.varL 
| CommandSeq of AST.comS 
| Command of AST.com
| Expression of AST.exp 
| IntExpression of AST.expI 
| IntTerm of AST.ter 
| IntFactor of AST.fac 
| BoolExpression of AST.expB 
| BoolTerm of AST.terB 
| BoolFactor of AST.facB 
| Comparison of AST.comp
| Variable of AST.var 
| RelOp of AST.rel 
| AddOp of AST.add 
| MultOp of AST.mul 
| Identifier  of AST.ide 
| Numeral of AST.num 
| Dummy1 of AST.dum 
| Dummy2 of AST.dumm




%pos int
 %arg (fileName) : string
(*optional declarations *)
%eop EOF
%noshift EOF

%start Program

%verbose

%%

Dummy2 : COMMA Variable Dummy2 (AST.dummy2( COMMA, Variable, Dummy2 ))
        | (empty2)
Dummy1 : Command SCOL Dummy1 (AST.dummy1(Command, SCOL, Dummy1))
        | (empty3)
Program : PROG Identifier DCOL Block (AST.program(PROG, Identifier, DCOL, Block))
Block : DeclarationSeq CommandSeq (AST.block(DeclarationSeq, CommandSeq))
DeclarationSeq : Declaration DeclarationSeq (AST.declarationSring(Declaration, DeclarationSeq))
        | (empty1)
Declaration : VAR VariableList COL Type SCOL (AST.declaration(VAR, VariableList, COL, Type, SCOL))
Type : INT (INT)
        | BOOL (BOOL)
VariableList: Variable (AST.variableList1(Variable))
        | Variable Dummy2 (AST.variableList2(Variable, Dummy2))
CommandSeq : CURL Dummy1 CURR (AST.commandS1(CURL, Dummy1, CURR))
Command : Variable IS Expression (AST.command1(Variable, IS, Expression))
        | READ Variable (AST.command2(READ, Variable))
        | WRITE IntExpression (AST.command3(WRITE, IntExpression))
        | IF BoolExpression THEN CommandSeq ELSE CommandSeq ENDIF(AST.command4(IF, BoolExpression, THEN, CommandSeq1, ELSE, CommandSeq2, ENDIF))
        | WHILE BoolExpression DO CommandSeq ENDWH(AST.command5(WHILE, BoolExpression, DO, CommandSeq, ENDWH))
Expression: IntExpression   (AST.expression1(IntExpression))
        | BoolExpression (AST.expression2(BoolExpression))
IntExpression : IntExpression AddOp IntTerm (AST.expressionI1( IntExpression, AddOp, IntTerm))
        | IntTerm (AST.expressionI2(IntTerm))
IntTerm : IntTerm MultOp IntFactor (AST.intterm1(IntTerm, MultOp, IntFactor))
        | IntFactor(AST.intterm2(IntFactor))
IntFactor: Numeral (AST.fac1(Numeral))
        | Variable (AST.fac2(Variable))
        | ROUNDL IntExpression ROUNDR (AST.fac3(ROUNDL, IntExpression, ROUNDR))
        | BY IntFactor (AST.fac4(BY, IntFactor))
BoolExpression: BoolExpression OR BoolTerm (AST.expressionB1(BoolExpression, OR, BoolTerm))
        | BoolTerm (AST.expressionB2(BoolTerm))
BoolTerm : BoolTerm DAND BoolFactor (AST.boolterm1( BoolTerm, DAND, BoolFactor))
        | BoolFactor(AST.boolterm2(BoolFactor))
BoolFactor: DT (AST.facB1(DT))
        | DF(AST.facB2(DF))
        |Variable(AST.facB3(Variable))
        |Comparison(AST.facB4(Comparison))
        | ROUNDL BoolExpression ROUNDR(AST.facB5(ROUNDL, BoolExpression, ROUNDR))
        | EXCLAMATION BoolFactor(AST.facB6(EXCLAMATION, BoolFactor))
Comparison : IntExpression RelOp IntExpression(AST.comparison(IntExpression, RelOp, IntExpression))
Variable : Identifier (AST.variable(Identifier))
RelOp : LESSER (AST.LESSER)
        | LESSEREQ (AST.LESSEREQ)
        | EQUALop (AST.EQUALop)
        | GREATERop (AST.GREATERop)
        | GREATEREQ (AST.GREATEREQ)
        | TRIANGLEBR (AST.TRIANGLEBR)
AddOp : ADD  (AST.ADD)
        | SUBTRACT  (AST.SUB)
MultOp : MULTIPLY  (AST.Mul)
        | DIVIDE (AST.Divi)
        | MOD  (AST.MOD)
Identifier : ID (AST.identifier(ID))
Numeral : NUM (AST.numeral(NUM))