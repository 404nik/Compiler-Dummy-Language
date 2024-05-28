functor WHILELrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : WHILE_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open AST

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\003\000\000\000\
\\001\000\002\000\006\000\000\000\
\\001\000\004\000\098\000\008\000\025\000\000\000\
\\001\000\004\000\026\000\000\000\
\\001\000\005\000\123\000\015\000\132\000\019\000\132\000\022\000\123\000\
\\023\000\132\000\026\000\132\000\029\000\123\000\030\000\123\000\
\\031\000\123\000\032\000\123\000\033\000\123\000\034\000\123\000\
\\035\000\123\000\036\000\123\000\037\000\123\000\038\000\123\000\
\\039\000\123\000\000\000\
\\001\000\005\000\029\000\000\000\
\\001\000\005\000\082\000\000\000\
\\001\000\006\000\053\000\007\000\052\000\000\000\
\\001\000\009\000\013\000\000\000\
\\001\000\010\000\027\000\000\000\
\\001\000\011\000\028\000\000\000\
\\001\000\015\000\079\000\023\000\059\000\000\000\
\\001\000\016\000\093\000\000\000\
\\001\000\017\000\095\000\000\000\
\\001\000\019\000\060\000\023\000\059\000\000\000\
\\001\000\020\000\092\000\000\000\
\\001\000\021\000\044\000\024\000\043\000\025\000\042\000\027\000\041\000\
\\028\000\040\000\040\000\005\000\041\000\039\000\000\000\
\\001\000\021\000\048\000\024\000\043\000\040\000\005\000\041\000\039\000\000\000\
\\001\000\022\000\089\000\023\000\059\000\000\000\
\\001\000\022\000\090\000\029\000\074\000\030\000\073\000\031\000\072\000\
\\032\000\071\000\033\000\070\000\034\000\069\000\035\000\068\000\
\\036\000\067\000\000\000\
\\001\000\022\000\090\000\035\000\068\000\036\000\067\000\000\000\
\\001\000\029\000\074\000\030\000\073\000\031\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\000\000\
\\001\000\040\000\005\000\000\000\
\\001\000\042\000\000\000\000\000\
\\097\000\000\000\
\\098\000\008\000\025\000\000\000\
\\099\000\000\000\
\\100\000\012\000\023\000\013\000\022\000\014\000\021\000\018\000\020\000\
\\040\000\005\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\003\000\010\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\035\000\068\000\036\000\067\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\029\000\074\000\030\000\073\000\031\000\072\000\032\000\071\000\
\\033\000\070\000\034\000\069\000\035\000\068\000\036\000\067\000\000\000\
\\117\000\023\000\059\000\000\000\
\\118\000\037\000\064\000\038\000\063\000\039\000\062\000\000\000\
\\119\000\037\000\064\000\038\000\063\000\039\000\062\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\026\000\058\000\000\000\
\\127\000\026\000\058\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\035\000\068\000\036\000\067\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\"
val actionRowNumbers =
"\000\000\022\000\001\000\074\000\
\\031\000\031\000\008\000\028\000\
\\022\000\030\000\029\000\027\000\
\\062\000\002\000\003\000\009\000\
\\010\000\005\000\016\000\016\000\
\\017\000\022\000\035\000\022\000\
\\007\000\036\000\016\000\027\000\
\\048\000\004\000\058\000\055\000\
\\053\000\014\000\047\000\045\000\
\\021\000\075\000\016\000\057\000\
\\056\000\017\000\016\000\011\000\
\\049\000\039\000\017\000\038\000\
\\025\000\006\000\034\000\033\000\
\\043\000\042\000\037\000\026\000\
\\016\000\016\000\008\000\017\000\
\\073\000\072\000\071\000\017\000\
\\017\000\070\000\069\000\068\000\
\\067\000\066\000\065\000\064\000\
\\063\000\060\000\051\000\018\000\
\\019\000\008\000\020\000\024\000\
\\032\000\054\000\052\000\015\000\
\\046\000\044\000\061\000\059\000\
\\050\000\012\000\041\000\008\000\
\\013\000\040\000\023\000"
val gotoT =
"\
\\001\000\094\000\000\000\
\\021\000\002\000\000\000\
\\000\000\
\\000\000\
\\002\000\007\000\003\000\006\000\004\000\005\000\000\000\
\\003\000\009\000\004\000\005\000\000\000\
\\007\000\010\000\000\000\
\\000\000\
\\006\000\014\000\017\000\013\000\021\000\012\000\000\000\
\\000\000\
\\000\000\
\\008\000\017\000\017\000\016\000\021\000\012\000\023\000\015\000\000\000\
\\000\000\
\\024\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\013\000\033\000\
\\014\000\032\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\021\000\012\000\022\000\028\000\000\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\013\000\043\000\
\\014\000\032\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\021\000\012\000\022\000\028\000\000\000\
\\010\000\045\000\011\000\035\000\012\000\034\000\017\000\044\000\
\\021\000\012\000\022\000\028\000\000\000\
\\017\000\047\000\021\000\012\000\000\000\
\\000\000\
\\017\000\048\000\021\000\012\000\000\000\
\\005\000\049\000\000\000\
\\000\000\
\\009\000\054\000\010\000\053\000\011\000\035\000\012\000\034\000\
\\013\000\052\000\014\000\032\000\015\000\031\000\016\000\030\000\
\\017\000\029\000\021\000\012\000\022\000\028\000\000\000\
\\008\000\017\000\017\000\016\000\021\000\012\000\023\000\055\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\059\000\000\000\
\\018\000\064\000\019\000\063\000\000\000\
\\000\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\015\000\073\000\
\\016\000\030\000\017\000\029\000\021\000\012\000\022\000\028\000\000\000\
\\000\000\
\\000\000\
\\012\000\074\000\017\000\044\000\021\000\012\000\022\000\028\000\000\000\
\\010\000\076\000\011\000\035\000\012\000\034\000\013\000\075\000\
\\014\000\032\000\015\000\031\000\016\000\030\000\017\000\029\000\
\\021\000\012\000\022\000\028\000\000\000\
\\000\000\
\\000\000\
\\019\000\063\000\000\000\
\\010\000\078\000\011\000\035\000\012\000\034\000\017\000\044\000\
\\021\000\012\000\022\000\028\000\000\000\
\\000\000\
\\024\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\064\000\019\000\063\000\000\000\
\\000\000\
\\000\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\015\000\081\000\
\\016\000\030\000\017\000\029\000\021\000\012\000\022\000\028\000\000\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\014\000\082\000\
\\015\000\031\000\016\000\030\000\017\000\029\000\021\000\012\000\
\\022\000\028\000\000\000\
\\007\000\083\000\000\000\
\\012\000\084\000\017\000\044\000\021\000\012\000\022\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\085\000\012\000\034\000\017\000\044\000\021\000\012\000\
\\022\000\028\000\000\000\
\\010\000\086\000\011\000\035\000\012\000\034\000\017\000\044\000\
\\021\000\012\000\022\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\064\000\019\000\063\000\000\000\
\\007\000\089\000\000\000\
\\019\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\059\000\000\000\
\\019\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\092\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 95
val numrules = 54
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | Dummy2 of unit ->  (AST.dumm) | Dummy1 of unit ->  (AST.dum)
 | Numeral of unit ->  (AST.num) | Identifier of unit ->  (AST.ide)
 | MultOp of unit ->  (AST.mul) | AddOp of unit ->  (AST.add)
 | RelOp of unit ->  (AST.rel) | Variable of unit ->  (AST.var)
 | Comparison of unit ->  (AST.comp)
 | BoolFactor of unit ->  (AST.facB) | BoolTerm of unit ->  (AST.terB)
 | BoolExpression of unit ->  (AST.expB)
 | IntFactor of unit ->  (AST.fac) | IntTerm of unit ->  (AST.ter)
 | IntExpression of unit ->  (AST.expI)
 | Expression of unit ->  (AST.exp) | Command of unit ->  (AST.com)
 | CommandSeq of unit ->  (AST.comS)
 | VariableList of unit ->  (AST.varL) | Type of unit ->  (AST.typ)
 | Declaration of unit ->  (AST.dec)
 | DeclarationSeq of unit ->  (AST.decS) | Block of unit ->  (AST.blo)
 | Program of unit ->  (AST.pro)
end
type svalue = MlyValue.svalue
type result = AST.pro
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "PROG"
  | (T 1) => "DCOL"
  | (T 2) => "VAR"
  | (T 3) => "COL"
  | (T 4) => "SCOL"
  | (T 5) => "INT"
  | (T 6) => "BOOL"
  | (T 7) => "COMMA"
  | (T 8) => "CURL"
  | (T 9) => "CURR"
  | (T 10) => "IS"
  | (T 11) => "READ"
  | (T 12) => "WRITE"
  | (T 13) => "IF"
  | (T 14) => "THEN"
  | (T 15) => "ELSE"
  | (T 16) => "ENDIF"
  | (T 17) => "WHILE"
  | (T 18) => "DO"
  | (T 19) => "ENDWH"
  | (T 20) => "ROUNDL"
  | (T 21) => "ROUNDR"
  | (T 22) => "OR"
  | (T 23) => "BY"
  | (T 24) => "DT"
  | (T 25) => "DAND"
  | (T 26) => "DF"
  | (T 27) => "EXCLAMATION"
  | (T 28) => "LESSER"
  | (T 29) => "LESSEREQ"
  | (T 30) => "EQUALop"
  | (T 31) => "GREATERop"
  | (T 32) => "GREATEREQ"
  | (T 33) => "TRIANGLEBR"
  | (T 34) => "ADD"
  | (T 35) => "SUBTRACT"
  | (T 36) => "MULTIPLY"
  | (T 37) => "DIVIDE"
  | (T 38) => "MOD"
  | (T 39) => "ID"
  | (T 40) => "NUM"
  | (T 41) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Dummy2 Dummy21, _, Dummy21right)) :: ( _, (
 MlyValue.Variable Variable1, _, _)) :: ( _, ( _, COMMA1left, _)) :: 
rest671)) => let val  result = MlyValue.Dummy2 (fn _ => let val  (
Variable as Variable1) = Variable1 ()
 val  (Dummy2 as Dummy21) = Dummy21 ()
 in (AST.dummy2( COMMA, Variable, Dummy2 ))
end)
 in ( LrTable.NT 23, ( result, COMMA1left, Dummy21right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.Dummy2 (fn _ => (
empty2))
 in ( LrTable.NT 23, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Dummy1 Dummy11, _, Dummy11right)) :: _ :: (
 _, ( MlyValue.Command Command1, Command1left, _)) :: rest671)) => let
 val  result = MlyValue.Dummy1 (fn _ => let val  (Command as Command1)
 = Command1 ()
 val  (Dummy1 as Dummy11) = Dummy11 ()
 in (AST.dummy1(Command, SCOL, Dummy1))
end)
 in ( LrTable.NT 22, ( result, Command1left, Dummy11right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.Dummy1 (fn _ => (
empty3))
 in ( LrTable.NT 22, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: _ :: ( _, 
( MlyValue.Identifier Identifier1, _, _)) :: ( _, ( _, PROG1left, _))
 :: rest671)) => let val  result = MlyValue.Program (fn _ => let val 
 (Identifier as Identifier1) = Identifier1 ()
 val  (Block as Block1) = Block1 ()
 in (AST.program(PROG, Identifier, DCOL, Block))
end)
 in ( LrTable.NT 0, ( result, PROG1left, Block1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.CommandSeq CommandSeq1, _, CommandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (AST.block(DeclarationSeq, CommandSeq))
end)
 in ( LrTable.NT 1, ( result, DeclarationSeq1left, CommandSeq1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, _, 
DeclarationSeq1right)) :: ( _, ( MlyValue.Declaration Declaration1, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.DeclarationSeq (fn _ => let val  (Declaration as Declaration1
) = Declaration1 ()
 val  (DeclarationSeq as DeclarationSeq1) = DeclarationSeq1 ()
 in (AST.declarationSring(Declaration, DeclarationSeq))
end)
 in ( LrTable.NT 2, ( result, Declaration1left, DeclarationSeq1right),
 rest671)
end
|  ( 7, ( rest671)) => let val  result = MlyValue.DeclarationSeq (fn _
 => (empty1))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( _, _, SCOL1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: _ :: ( _, ( MlyValue.VariableList VariableList1, _, _)) :: ( _
, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Declaration (fn _ => let val  (VariableList as VariableList1)
 = VariableList1 ()
 val  (Type as Type1) = Type1 ()
 in (AST.declaration(VAR, VariableList, COL, Type, SCOL))
end)
 in ( LrTable.NT 3, ( result, VAR1left, SCOL1right), rest671)
end
|  ( 9, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (INT))
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 10, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (BOOL))
 in ( LrTable.NT 4, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = 
MlyValue.VariableList (fn _ => let val  (Variable as Variable1) = 
Variable1 ()
 in (AST.variableList1(Variable))
end)
 in ( LrTable.NT 5, ( result, Variable1left, Variable1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.Dummy2 Dummy21, _, Dummy21right)) :: ( _, (
 MlyValue.Variable Variable1, Variable1left, _)) :: rest671)) => let
 val  result = MlyValue.VariableList (fn _ => let val  (Variable as 
Variable1) = Variable1 ()
 val  (Dummy2 as Dummy21) = Dummy21 ()
 in (AST.variableList2(Variable, Dummy2))
end)
 in ( LrTable.NT 5, ( result, Variable1left, Dummy21right), rest671)

end
|  ( 13, ( ( _, ( _, _, CURR1right)) :: ( _, ( MlyValue.Dummy1 Dummy11
, _, _)) :: ( _, ( _, CURL1left, _)) :: rest671)) => let val  result =
 MlyValue.CommandSeq (fn _ => let val  (Dummy1 as Dummy11) = Dummy11
 ()
 in (AST.commandS1(CURL, Dummy1, CURR))
end)
 in ( LrTable.NT 6, ( result, CURL1left, CURR1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Expression Expression1, _, Expression1right
)) :: _ :: ( _, ( MlyValue.Variable Variable1, Variable1left, _)) :: 
rest671)) => let val  result = MlyValue.Command (fn _ => let val  (
Variable as Variable1) = Variable1 ()
 val  (Expression as Expression1) = Expression1 ()
 in (AST.command1(Variable, IS, Expression))
end)
 in ( LrTable.NT 7, ( result, Variable1left, Expression1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.Variable Variable1, _, Variable1right)) :: 
( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (Variable as Variable1) = Variable1
 ()
 in (AST.command2(READ, Variable))
end)
 in ( LrTable.NT 7, ( result, READ1left, Variable1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.IntExpression IntExpression1, _, 
IntExpression1right)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let
 val  result = MlyValue.Command (fn _ => let val  (IntExpression as 
IntExpression1) = IntExpression1 ()
 in (AST.command3(WRITE, IntExpression))
end)
 in ( LrTable.NT 7, ( result, WRITE1left, IntExpression1right), 
rest671)
end
|  ( 17, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq2, _, _)) :: _ :: ( _, ( MlyValue.CommandSeq CommandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, _, _)) ::
 ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Command (fn _ => let val  (BoolExpression as BoolExpression1)
 = BoolExpression1 ()
 val  CommandSeq1 = CommandSeq1 ()
 val  CommandSeq2 = CommandSeq2 ()
 in (
AST.command4(IF, BoolExpression, THEN, CommandSeq1, ELSE, CommandSeq2, ENDIF)
)
end)
 in ( LrTable.NT 7, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.CommandSeq 
CommandSeq1, _, _)) :: _ :: ( _, ( MlyValue.BoolExpression 
BoolExpression1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.Command (fn _ => let val  (BoolExpression
 as BoolExpression1) = BoolExpression1 ()
 val  (CommandSeq as CommandSeq1) = CommandSeq1 ()
 in (AST.command5(WHILE, BoolExpression, DO, CommandSeq, ENDWH))
end)
 in ( LrTable.NT 7, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.IntExpression IntExpression1, 
IntExpression1left, IntExpression1right)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => let val  (IntExpression as 
IntExpression1) = IntExpression1 ()
 in (AST.expression1(IntExpression))
end)
 in ( LrTable.NT 8, ( result, IntExpression1left, IntExpression1right)
, rest671)
end
|  ( 20, ( ( _, ( MlyValue.BoolExpression BoolExpression1, 
BoolExpression1left, BoolExpression1right)) :: rest671)) => let val  
result = MlyValue.Expression (fn _ => let val  (BoolExpression as 
BoolExpression1) = BoolExpression1 ()
 in (AST.expression2(BoolExpression))
end)
 in ( LrTable.NT 8, ( result, BoolExpression1left, 
BoolExpression1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.IntTerm IntTerm1, _, IntTerm1right)) :: ( _
, ( MlyValue.AddOp AddOp1, _, _)) :: ( _, ( MlyValue.IntExpression 
IntExpression1, IntExpression1left, _)) :: rest671)) => let val  
result = MlyValue.IntExpression (fn _ => let val  (IntExpression as 
IntExpression1) = IntExpression1 ()
 val  (AddOp as AddOp1) = AddOp1 ()
 val  (IntTerm as IntTerm1) = IntTerm1 ()
 in (AST.expressionI1( IntExpression, AddOp, IntTerm))
end)
 in ( LrTable.NT 9, ( result, IntExpression1left, IntTerm1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.IntTerm IntTerm1, IntTerm1left, 
IntTerm1right)) :: rest671)) => let val  result = 
MlyValue.IntExpression (fn _ => let val  (IntTerm as IntTerm1) = 
IntTerm1 ()
 in (AST.expressionI2(IntTerm))
end)
 in ( LrTable.NT 9, ( result, IntTerm1left, IntTerm1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: ( _, ( MlyValue.MultOp MultOp1, _, _)) :: ( _, ( MlyValue.IntTerm 
IntTerm1, IntTerm1left, _)) :: rest671)) => let val  result = 
MlyValue.IntTerm (fn _ => let val  (IntTerm as IntTerm1) = IntTerm1 ()
 val  (MultOp as MultOp1) = MultOp1 ()
 val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (AST.intterm1(IntTerm, MultOp, IntFactor))
end)
 in ( LrTable.NT 10, ( result, IntTerm1left, IntFactor1right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.IntFactor IntFactor1, IntFactor1left, 
IntFactor1right)) :: rest671)) => let val  result = MlyValue.IntTerm
 (fn _ => let val  (IntFactor as IntFactor1) = IntFactor1 ()
 in (AST.intterm2(IntFactor))
end)
 in ( LrTable.NT 10, ( result, IntFactor1left, IntFactor1right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.Numeral Numeral1, Numeral1left, 
Numeral1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (Numeral as Numeral1) = Numeral1 ()
 in (AST.fac1(Numeral))
end)
 in ( LrTable.NT 11, ( result, Numeral1left, Numeral1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.IntFactor
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in (AST.fac2(Variable))
end)
 in ( LrTable.NT 11, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 27, ( ( _, ( _, _, ROUNDR1right)) :: ( _, ( 
MlyValue.IntExpression IntExpression1, _, _)) :: ( _, ( _, ROUNDL1left
, _)) :: rest671)) => let val  result = MlyValue.IntFactor (fn _ =>
 let val  (IntExpression as IntExpression1) = IntExpression1 ()
 in (AST.fac3(ROUNDL, IntExpression, ROUNDR))
end)
 in ( LrTable.NT 11, ( result, ROUNDL1left, ROUNDR1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.IntFactor IntFactor1, _, IntFactor1right))
 :: ( _, ( _, BY1left, _)) :: rest671)) => let val  result = 
MlyValue.IntFactor (fn _ => let val  (IntFactor as IntFactor1) = 
IntFactor1 ()
 in (AST.fac4(BY, IntFactor))
end)
 in ( LrTable.NT 11, ( result, BY1left, IntFactor1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.BoolTerm BoolTerm1, _, BoolTerm1right)) ::
 _ :: ( _, ( MlyValue.BoolExpression BoolExpression1, 
BoolExpression1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (BoolExpression as 
BoolExpression1) = BoolExpression1 ()
 val  (BoolTerm as BoolTerm1) = BoolTerm1 ()
 in (AST.expressionB1(BoolExpression, OR, BoolTerm))
end)
 in ( LrTable.NT 12, ( result, BoolExpression1left, BoolTerm1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.BoolTerm BoolTerm1, BoolTerm1left, 
BoolTerm1right)) :: rest671)) => let val  result = 
MlyValue.BoolExpression (fn _ => let val  (BoolTerm as BoolTerm1) = 
BoolTerm1 ()
 in (AST.expressionB2(BoolTerm))
end)
 in ( LrTable.NT 12, ( result, BoolTerm1left, BoolTerm1right), rest671
)
end
|  ( 31, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: _ :: ( _, ( MlyValue.BoolTerm BoolTerm1, BoolTerm1left, _)) :: 
rest671)) => let val  result = MlyValue.BoolTerm (fn _ => let val  (
BoolTerm as BoolTerm1) = BoolTerm1 ()
 val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (AST.boolterm1( BoolTerm, DAND, BoolFactor))
end)
 in ( LrTable.NT 13, ( result, BoolTerm1left, BoolFactor1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.BoolFactor BoolFactor1, BoolFactor1left, 
BoolFactor1right)) :: rest671)) => let val  result = MlyValue.BoolTerm
 (fn _ => let val  (BoolFactor as BoolFactor1) = BoolFactor1 ()
 in (AST.boolterm2(BoolFactor))
end)
 in ( LrTable.NT 13, ( result, BoolFactor1left, BoolFactor1right), 
rest671)
end
|  ( 33, ( ( _, ( _, DT1left, DT1right)) :: rest671)) => let val  
result = MlyValue.BoolFactor (fn _ => (AST.facB1(DT)))
 in ( LrTable.NT 14, ( result, DT1left, DT1right), rest671)
end
|  ( 34, ( ( _, ( _, DF1left, DF1right)) :: rest671)) => let val  
result = MlyValue.BoolFactor (fn _ => (AST.facB2(DF)))
 in ( LrTable.NT 14, ( result, DF1left, DF1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Variable Variable1, Variable1left, 
Variable1right)) :: rest671)) => let val  result = MlyValue.BoolFactor
 (fn _ => let val  (Variable as Variable1) = Variable1 ()
 in (AST.facB3(Variable))
end)
 in ( LrTable.NT 14, ( result, Variable1left, Variable1right), rest671
)
end
|  ( 36, ( ( _, ( MlyValue.Comparison Comparison1, Comparison1left, 
Comparison1right)) :: rest671)) => let val  result = 
MlyValue.BoolFactor (fn _ => let val  (Comparison as Comparison1) = 
Comparison1 ()
 in (AST.facB4(Comparison))
end)
 in ( LrTable.NT 14, ( result, Comparison1left, Comparison1right), 
rest671)
end
|  ( 37, ( ( _, ( _, _, ROUNDR1right)) :: ( _, ( 
MlyValue.BoolExpression BoolExpression1, _, _)) :: ( _, ( _, 
ROUNDL1left, _)) :: rest671)) => let val  result = MlyValue.BoolFactor
 (fn _ => let val  (BoolExpression as BoolExpression1) = 
BoolExpression1 ()
 in (AST.facB5(ROUNDL, BoolExpression, ROUNDR))
end)
 in ( LrTable.NT 14, ( result, ROUNDL1left, ROUNDR1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.BoolFactor BoolFactor1, _, BoolFactor1right
)) :: ( _, ( _, EXCLAMATION1left, _)) :: rest671)) => let val  result
 = MlyValue.BoolFactor (fn _ => let val  (BoolFactor as BoolFactor1) =
 BoolFactor1 ()
 in (AST.facB6(EXCLAMATION, BoolFactor))
end)
 in ( LrTable.NT 14, ( result, EXCLAMATION1left, BoolFactor1right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.IntExpression IntExpression2, _, 
IntExpression2right)) :: ( _, ( MlyValue.RelOp RelOp1, _, _)) :: ( _, 
( MlyValue.IntExpression IntExpression1, IntExpression1left, _)) :: 
rest671)) => let val  result = MlyValue.Comparison (fn _ => let val  (
IntExpression as IntExpression1) = IntExpression1 ()
 val  (RelOp as RelOp1) = RelOp1 ()
 val  IntExpression2 = IntExpression2 ()
 in (AST.comparison(IntExpression, RelOp, IntExpression))
end)
 in ( LrTable.NT 15, ( result, IntExpression1left, IntExpression2right
), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Identifier Identifier1, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Variable
 (fn _ => let val  (Identifier as Identifier1) = Identifier1 ()
 in (AST.variable(Identifier))
end)
 in ( LrTable.NT 16, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 41, ( ( _, ( _, LESSER1left, LESSER1right)) :: rest671)) => let
 val  result = MlyValue.RelOp (fn _ => (AST.LESSER))
 in ( LrTable.NT 17, ( result, LESSER1left, LESSER1right), rest671)

end
|  ( 42, ( ( _, ( _, LESSEREQ1left, LESSEREQ1right)) :: rest671)) =>
 let val  result = MlyValue.RelOp (fn _ => (AST.LESSEREQ))
 in ( LrTable.NT 17, ( result, LESSEREQ1left, LESSEREQ1right), rest671
)
end
|  ( 43, ( ( _, ( _, EQUALop1left, EQUALop1right)) :: rest671)) => let
 val  result = MlyValue.RelOp (fn _ => (AST.EQUALop))
 in ( LrTable.NT 17, ( result, EQUALop1left, EQUALop1right), rest671)

end
|  ( 44, ( ( _, ( _, GREATERop1left, GREATERop1right)) :: rest671)) =>
 let val  result = MlyValue.RelOp (fn _ => (AST.GREATERop))
 in ( LrTable.NT 17, ( result, GREATERop1left, GREATERop1right), 
rest671)
end
|  ( 45, ( ( _, ( _, GREATEREQ1left, GREATEREQ1right)) :: rest671)) =>
 let val  result = MlyValue.RelOp (fn _ => (AST.GREATEREQ))
 in ( LrTable.NT 17, ( result, GREATEREQ1left, GREATEREQ1right), 
rest671)
end
|  ( 46, ( ( _, ( _, TRIANGLEBR1left, TRIANGLEBR1right)) :: rest671))
 => let val  result = MlyValue.RelOp (fn _ => (AST.TRIANGLEBR))
 in ( LrTable.NT 17, ( result, TRIANGLEBR1left, TRIANGLEBR1right), 
rest671)
end
|  ( 47, ( ( _, ( _, ADD1left, ADD1right)) :: rest671)) => let val  
result = MlyValue.AddOp (fn _ => (AST.ADD))
 in ( LrTable.NT 18, ( result, ADD1left, ADD1right), rest671)
end
|  ( 48, ( ( _, ( _, SUBTRACT1left, SUBTRACT1right)) :: rest671)) =>
 let val  result = MlyValue.AddOp (fn _ => (AST.SUB))
 in ( LrTable.NT 18, ( result, SUBTRACT1left, SUBTRACT1right), rest671
)
end
|  ( 49, ( ( _, ( _, MULTIPLY1left, MULTIPLY1right)) :: rest671)) =>
 let val  result = MlyValue.MultOp (fn _ => (AST.Mul))
 in ( LrTable.NT 19, ( result, MULTIPLY1left, MULTIPLY1right), rest671
)
end
|  ( 50, ( ( _, ( _, DIVIDE1left, DIVIDE1right)) :: rest671)) => let
 val  result = MlyValue.MultOp (fn _ => (AST.Divi))
 in ( LrTable.NT 19, ( result, DIVIDE1left, DIVIDE1right), rest671)

end
|  ( 51, ( ( _, ( _, MOD1left, MOD1right)) :: rest671)) => let val  
result = MlyValue.MultOp (fn _ => (AST.MOD))
 in ( LrTable.NT 19, ( result, MOD1left, MOD1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Identifier (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (AST.identifier(ID))
end)
 in ( LrTable.NT 20, ( result, ID1left, ID1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.Numeral (fn _ => let val  (NUM as NUM1
) = NUM1 ()
 in (AST.numeral(NUM))
end)
 in ( LrTable.NT 21, ( result, NUM1left, NUM1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : WHILE_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun PROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DCOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SCOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun CURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun CURR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ROUNDL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun ROUNDR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun BY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun DT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun DAND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun DF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCLAMATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSEREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALop (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERop (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEREQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TRIANGLEBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun SUBTRACT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTIPLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
