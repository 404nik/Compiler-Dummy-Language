val mem = Array.array(25,0);

fun lick (filename:string) =
 let val f = TextIO.getInstream(TextIO.openIn filename)
fun loop (accum: string list, f) =
 case (TextIO.StreamIO.inputLine f) of
 SOME(chunk, f') => loop (chunk::accum, f')
 | NONE => (TextIO.StreamIO.closeIn f; accum)
 (* esac *)
 in rev(loop ([], f))
 end

fun chomp1 s =
 let val charlist = rev (explode s)
fun nibble [] = []
 | nibble (#"\n"::l) = l
 | nibble l = l
 in implode (rev (nibble charlist))
 end
fun chomp (L: string list) = map chomp1 L;
val readLines = chomp o lick
(* lick and chomp1 and codes provided by Prof Arun Kumar Sir *)

(* Helper function which takes input from user *)
fun getNumber() = (
print "Input: ";
let
    val a = valOf (TextIO.inputLine TextIO.stdIn)
    val b : int = valOf (Int.fromString a)
in
    b
end
);
 
(* getres is a fuction which takes 4 inputs as list of 4-int and does various operations as 
specified by opcodes.pdf   *)
(* Array.sub, Array.update, raise Empty are in built in SML  *) 
fun getres([a,b,c,d])= 
    if a =0 then OS.Process.exit(OS.Process.success)
    else if a = 1 then let val x = getNumber() in Array.update(mem,d,x) end 
    else if a = 2 then let val temp = Array.sub(mem,b) in Array.update(mem,d,temp) end 
    else if a = 3 then if b = 1 then Array.update(mem,d,0) else Array.update(mem,d,1)  
    else if a = 4 then if b = 1 orelse c =1 then Array.update(mem,d,1) else Array.update(mem,d,0) 
    else if a = 5 then if b = 1 andalso c = 1 then Array.update(mem,d,1) else Array.update(mem,d,0)  
    else if a = 6 then let val x = (Array.sub(mem,b) + Array.sub(mem,c)) in Array.update(mem,d,x) end  
    else if a = 7 then let val x = (Array.sub(mem,b) - Array.sub(mem,c)) in Array.update(mem,d,x) end 
    else if a = 8 then let val x = (Array.sub(mem,b)*  Array.sub(mem,c)) in Array.update(mem,d,x) end 
    else if a = 9 then let val x = (Array.sub(mem,b)div  Array.sub(mem,c)) in Array.update(mem,d,x) end 
    else if a = 10 then let val x = (Array.sub(mem,b) mod Array.sub(mem,c)) in Array.update(mem,d,x) end 
    else if a = 11 then if (Array.sub(mem,b) = Array.sub(mem,c)) then Array.update(mem,d,1) else Array.update(mem,d,0) 
    else if a = 12 then if (Array.sub(mem,b) > Array.sub(mem,c)) then Array.update(mem,d,1) else Array.update(mem,d,0) 
    else if a = 15 then print( Int.toString(Array.sub(mem,b)))
    else if a= 16 then Array.update(mem,d,b)
    else if a = 13 then Array.update(mem,0,Array.sub(mem,0))
    else if a = 14 then Array.update(mem,0,Array.sub(mem,0))
    else raise Empty;

(* get list fuction gets the list of list of int from list of string where the string is tuple of 4 numbers ,List.nth(v,i) gets the ith element of 
list v , String. tokens helps in removing "," and "(" characters , function can be applied to all elements of list using List.mapPartial*) 
fun getlist(v, i) = if  (i) = length (v)-1 then let val  y = List.nth(v,i) in  
     [List.mapPartial Int.fromString(String.tokens(fn y => y = #"," orelse y= #"(") y)] end
     else let val  y = List.nth(v,i) in  
     List.mapPartial Int.fromString(String.tokens(fn y => y = #"," orelse y= #"(") y) :: getlist((v), i+1)end ;

(* Finally the interpret fucntion take the input file of operation of choice and outputs the desired result using computation step followed in _.bdim files*)
fun interpret(filename:string):unit =
    let val a = getlist(readLines(filename),0)
        fun abc(c) = 
            let 
                val p = List.nth(a,c) 
                val b= getres(p)
                val q = print(Int.toString c)
            in 
                if ((List.nth(p,0) = 13 andalso Array.sub(mem,List.nth(p,1)) = 1) orelse List.nth(p,0)=14) then abc(List.nth(p,3))
                else abc(c+1)
            end
    in
        abc(0)
end;

