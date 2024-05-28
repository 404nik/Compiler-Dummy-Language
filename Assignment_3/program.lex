structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
    type lexarg = string
        type arg = lexarg
  exception badCharacter
  val pos = ref 1
  val c = ref 1
  val out = ref "["
  val eof = fn fileName => (Tokens.EOF(!pos, !pos))
  val error = fn (e, l:int, c:int) => TextIO.output(TextIO.stdOut,"Unknown Token:"^Int.toString(l)^":"^Int.toString(c)^":"^e)
  val giveCol = fn () => !c
  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
%%
%full
%header (functor WHILELexFun(structure Tokens:WHILE_TOKENS));
%arg (fileName:string);
alpha=[A-Za-z];
numer=[A-Za-z0-9];
number=[0-9];
ws = [\ \t\n\r];

%%
"program"   => (c := !c + 7 ; out := !out^"CONST \"TRUE\","; Tokens.PROG(!pos,!pos));
"::"  => (c := !c + 2 ; out := !out^"CONST \"FALSE\","; Tokens.DCOL(!pos,!pos));
"var"    => (c := !c + 3 ; out := !out^"AND \"AND\","; Tokens.VAR(!pos,!pos));
":"     => (c := !c + 1 ; out := !out^"OR \"OR\","; Tokens.COL(!pos,!pos));
";"    => (c := !c + 1 ; out := !out^"XOR \"XOR\","; Tokens.SCOL(!pos,!pos));
"int"    => (c := !c + 3 ; out := !out^"NOT \"NOT\","; Tokens.INT(!pos,!pos));
"bool" => (c := !c + 4 ; out := !out^"EQUALS \"EQUALS\","; Tokens.BOOL(!pos,!pos));
"," => (c := !c + 1 ;out := !out^"IMPLIES \"IMPLIES\","; Tokens.COMMA(!pos,!pos));
"{"     => (c := !c + 1 ; out := !out^"IF \"IF\","; Tokens.CURL(!pos,!pos));
"}"   => (c := !c + 1 ; out := !out^"THEN \"THEN\","; Tokens.CURR(!pos,!pos));
":="   => (c := !c + 2 ; out := !out^"ELSE \"ELSE\","; Tokens.IS(!pos,!pos));
"read"      => (c := !c + 4 ; out := !out^"LPAREN \"(\","; Tokens.READ(!pos,!pos));
"write"      => (c := !c + 5 ; out := !out^"RPAREN \")\","; Tokens.WRITE(!pos,!pos));
"if"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.IF(!pos,!pos));
"then"      => (c := !c + 4 ; out := !out^"RPAREN \")\","; Tokens.THEN(!pos,!pos));
"else"      => (c := !c + 4 ; out := !out^"RPAREN \")\","; Tokens.ELSE(!pos,!pos));
"endif"      => (c := !c + 5 ; out := !out^"RPAREN \")\","; Tokens.ENDIF(!pos,!pos));
"while"      => (c := !c + 5 ; out := !out^"RPAREN \")\","; Tokens.WHILE(!pos,!pos));
"do"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.DO(!pos,!pos));
"endwh"      => (c := !c + 5 ; out := !out^"RPAREN \")\","; Tokens.ENDWH(!pos,!pos));
"("      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.ROUNDL(!pos,!pos));
")"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.ROUNDR(!pos,!pos));
"||"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.OR(!pos,!pos));
"~"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.BY(!pos,!pos));
"&&"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.DAND(!pos,!pos));
"tt"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.DT(!pos,!pos));
"ff"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.DF(!pos,!pos));
"!"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.EXCLAMATION(!pos,!pos));
"<"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.LESSER(!pos,!pos));
"<="      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.LESSEREQ(!pos,!pos));
"="      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.EQUALop(!pos,!pos));
">"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.GREATERop(!pos,!pos));
">="      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.GREATEREQ(!pos,!pos));
"<>"      => (c := !c + 2 ; out := !out^"RPAREN \")\","; Tokens.TRIANGLEBR(!pos,!pos));
"+"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.ADD(!pos,!pos));
"-"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.SUBTRACT(!pos,!pos));
"*"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.MULTIPLY(!pos,!pos));
"/"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.DIVIDE(!pos,!pos));
"%"      => (c := !c + 1 ; out := !out^"RPAREN \")\","; Tokens.MOD(!pos,!pos));
{ws} => (c:= !c + 1 ; continue());
{alpha}{numer}* => (c := !c + size(yytext) ; out := !out^"ID \""^yytext^"\","; Tokens.ID(yytext,!pos,!pos));
{number}+ => (c := !c + size(yytext) ; out := !out^"NUM \""^yytext^"\","; Tokens.NUM (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !pos, !pos));
.        => (error(yytext,!pos,!c); out := "["; c := 1; pos := 1; raise badCharacter; continue());