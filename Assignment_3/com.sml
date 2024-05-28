structure WHILE :
sig val compile : string -> AST.pro
end =
struct
exception WHILEError;
fun compile (fileName) =
let val inStream = TextIO.openIn fileName;
val grab : int -> string = fn
n => if TextIO.endOfStream inStream
then ""
else TextIO.inputN (inStream,n);
val printError : string * int * int -> unit = fn
(msg,line,col) =>
print (fileName^"["^Int.toString line^":"
^Int.toString col^"] "^msg^"\n");
val (tree,rem) = WHILEParser.parse
(15,
(WHILEParser.makeLexer grab fileName),
printError,
fileName)
handle WHILEParser.ParseError => raise WHILEError;
(* Close the source program file *)
val _ = TextIO.closeIn inStream;
in tree
end
end;