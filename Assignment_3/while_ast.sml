structure AST = 
struct
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
           | empty2
 and dum = dummy1 of com * scol * dum
           | empty3  
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
 end;