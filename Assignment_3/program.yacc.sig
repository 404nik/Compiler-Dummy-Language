signature WHILE_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val MULTIPLY:  'a * 'a -> (svalue,'a) token
val SUBTRACT:  'a * 'a -> (svalue,'a) token
val ADD:  'a * 'a -> (svalue,'a) token
val TRIANGLEBR:  'a * 'a -> (svalue,'a) token
val GREATEREQ:  'a * 'a -> (svalue,'a) token
val GREATERop:  'a * 'a -> (svalue,'a) token
val EQUALop:  'a * 'a -> (svalue,'a) token
val LESSEREQ:  'a * 'a -> (svalue,'a) token
val LESSER:  'a * 'a -> (svalue,'a) token
val EXCLAMATION:  'a * 'a -> (svalue,'a) token
val DF:  'a * 'a -> (svalue,'a) token
val DAND:  'a * 'a -> (svalue,'a) token
val DT:  'a * 'a -> (svalue,'a) token
val BY:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val ROUNDR:  'a * 'a -> (svalue,'a) token
val ROUNDL:  'a * 'a -> (svalue,'a) token
val ENDWH:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val ENDIF:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val WRITE:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val IS:  'a * 'a -> (svalue,'a) token
val CURR:  'a * 'a -> (svalue,'a) token
val CURL:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val SCOL:  'a * 'a -> (svalue,'a) token
val COL:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val DCOL:  'a * 'a -> (svalue,'a) token
val PROG:  'a * 'a -> (svalue,'a) token
end
signature WHILE_LRVALS=
sig
structure Tokens : WHILE_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
