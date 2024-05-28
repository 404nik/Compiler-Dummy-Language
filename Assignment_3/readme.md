 -------------------- COL226 ASSIGNMENT 3 ----------------------
 - CONTEXT FREE GRAMMER 
 HERE WE FIRST DEFINE ALL THE TERMINAL SYMBOLS ALONG WITH ID AND NUM OF OF TYPES STRING AND INTEGER
TERMINALS -- 

PROG  | DCOL | VAR | COL | SCOL 
| INT | BOOL  | COMMA  | CURL  | CURR  | IS | READ | WRITE | IF 
| THEN  | ELSE  | ENDIF  | WHILE  | DO  | ENDWH 
| ROUNDL  | ROUNDR  | OR  | BY  | DT  | DAND  | DF 
| EXCLAMATION| LESSER| LESSEREQ  | EQUALop | GREATERop  | GREATEREQ  | TRIANGLEBR  | ADD 
| SUBTRACT  | MULTIPLY | DIVIDE | MOD | ID of string  | NUM of int | EOF

 
NEXT WE DEFINE THE NON TERMINALS BY REFFERING THEM WITH AST. 
NON TERMINALS --
Program of AST.pro | Block of AST.blo | DeclarationSeq of AST.decS | Declaration of AST.dec | Type of AST.typ | VariableList of AST.varL | CommandSeq of AST.comS | Command of AST.com| Expression of AST.exp | IntExpression of AST.expI | IntTerm of AST.ter| IntFactor of AST.fac | BoolExpression of AST.expB | BoolTerm of AST.terB | BoolFactor of AST.facB | Comparison of AST.comp| Variable of AST.var | RelOp of AST.rel | AddOp of AST.add | MultOp of AST.mul | Identifier  of AST.ide | Numeral of AST.num | Dummy1 of AST.dum | Dummy2 of AST.dumm


NEXT WE DEFINE THE CONTEXT FREE GRAMMER -- 

Dummy2 : COMMA Variable Dummy2 
Dummy1 : Command SCOL Dummy1 
Program : PROG Identifier DCOL Block 
Block : DeclarationSeq CommandSeq 
DeclarationSeq : Declaration DeclarationSeq 
Declaration : VAR VariableList COL Type SCOL
Type : INT (INT)
        | BOOL (BOOL)
VariableList: Variable 
        | Variable Dummy2 
CommandSeq : CURL Dummy1 CURR 
Command : Variable IS Expression 
        | READ Variable
        | WRITE IntExpression 
        | IF BoolExpression THEN CommandSeq ELSE CommandSeq ENDIF
        | WHILE BoolExpression DO CommandSeq ENDWH
Expression: IntExpression   
        | BoolExpression 
IntExpression : IntExpression AddOp IntTerm 
        | IntTerm 
IntTerm : IntTerm MultOp IntFactor 
        | IntFactor
IntFactor: Numeral 
        | Variable 
        | ROUNDL IntExpression ROUNDR 
        | BY IntFactor 
BoolExpression: BoolExpression OR BoolTerm 
        | BoolTerm 
BoolTerm : BoolTerm DAND BoolFactor 
        | BoolFactor
BoolFactor: DT 
        | DF
        |Variable
        |Comparison
        | ROUNDL BoolExpression ROUNDR
        | EXCLAMATION BoolFactor
Comparison : IntExpression RelOp IntExpression
Variable : Identifier
RelOp : LESSER 
        | LESSEREQ 
        | EQUALop
        | GREATERop
        | GREATEREQ 
        | TRIANGLEBR 
AddOp : ADD  
        | SUBTRACT 
MultOp : MULTIPLY 
        | DIVIDE
        | MOD 
Identifier : ID 
Numeral : NUM 
HERE dummy1 AND dummy2 ARE THE TEMPERORY NON TERMINALS DEFINED FOR RED CURLY BRACKETS. HERE WE DEFINE EACH NON TERMINALS WITH CORRESPONDING PRODUCTION RULES AS DEFINED IN THE HYPER NOTES 4.6.

NOW FURTHER THESE VARIABLES HAVE BEEN LINKED TO DATATYPES DEFINED IN "while_ast.sml".

- AST datatype defination 
HERE I HAVE FIRST DEFINED THE REFERENCE DATATYPES FOR ALL THE TERMINALS TO DIRECTLY USE THEM IN DEFINING DATATYPES. 
THEN I HAVE DEFINED ALL THE DATA TYPES OF NON TERMINALS LINKING AMONG EACH OTEHR AND TERMINALS. 

datatype of PROG is PROG * IDENTIFIER * SET * BLK 
then I have defined datatype of BLK  -> SEQ * SEQ 
Similary I have defined the other datatypes .

datatype exal = EXCLAMATION
datatype roundl = ROUNDL
datatype roundr = ROUNDR
datatype dt = DT
datatype df = DF
datatype by = BY 
datatype dand = DAND
datatype or= OR
datatype curl = CURL
datatype curr = CURR
datatype readd = READ
datatype write = WRITE 
datatype whilee = WHILE 
datatype doo = DO
datatype endwh = ENDWH
datatype iff = IF 
datatype thenn = THEN 
datatype elsee = ELSE 
datatype endiff = ENDIF
datatype iss = IS
datatype comma = COMMA 
datatype scol = SCOL 
datatype col = COL
datatype prog = PROG
datatype dcol = DCOL 
datatype va = VAR 

datatype pro = program of  prog* ide * dcol * blo
 and blo  = block of  decS * comS 
 and dec = declaration  of  va * varL * col * typ * scol 
 and decS  = declarationSring  of dec * decS 
    | empty1
 and varL  = variableList1 of var  
        | variableList2 of var * dumm
 and  typ  = INT | BOOL
 and  var = variable of ide 
 and ide = identifier of string
 and dumm = dummy2 of comma * var *  dumm 
 and dum = dummy1 of com * scol * dum 
 and com = command1 of var * iss * exp 
        | command2 of readd * var 
        | command3 of write * expI
        | command4 of iff * expB * thenn * comS * elsee * comS * endiff 
        | command5 of whilee * expB * doo * comS * endwh 
 and comS = commandS1 of curl * dum * curr 
 and exp = expression1 of expI 
        | expression2 of expB
 and expI = expressionI1 of expI * add * ter 
        | expressionI2 of ter 
 and expB = expressionB1 of expB * or * terB 
        | expressionB2 of terB
 and ter = intterm1 of ter * mul * fac 
        | intterm2 of fac 
 and terB = boolterm1 of terB * dand * facB 
        | boolterm2 of facB
 and fac = fac1 of num 
        | fac2 of var 
        | fac3 of roundl * expI * roundr 
        | fac4 of  by * fac  
 and facB = facB1 of dt 
        | facB2 of df 
        | facB3 of var 
        | facB4 of comp 
        | facB5 of roundl * expB * roundr 
        | facB6 of exal * facB  
 and comp = comparison of expI  *  rel *  expI
 and rel = LESSER| LESSEREQ | EQUALop | GREATERop | GREATEREQ | TRIANGLEBR
 and add = ADD
    | SUB
 and mul = MOD
    | Mul
    | Divi
 and num = numeral of int 

HERE TO COMPILE THE PROGRAM DO FOLLOWING IN TERMINAL- 
sml then press enter 
then type
CM.make "while_ast.cm";

Acknowledgements - 
HERE I HAVE USED com.sml, g.sml , while_ast.sml from ug.pdf provided by Arun Kumar Sir. 



