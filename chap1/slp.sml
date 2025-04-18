type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
	      PrintStm[IdExp "b"]))

(*

Write an ML function that tells the maximum number of arguments of any 
Print statement within any subexpression of the given statement. 
For example, maxargs(prog) is 2.
*)

fun maxargs x =
    case x of
	CompoundStm (x', y') => (maxargs x') + (maxargs y')
      | AssignStm (_,_) => 0
      | PrintStm _ => 1

maxargs prog			  
