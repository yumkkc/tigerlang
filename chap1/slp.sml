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

val one = maxargs prog


(*
Write an ML function interp: stm -> unit that interprets a rpgogram in this language. To write in a functional style
without assignment, or arrays - maintain a list of variable, integer pairs and produce new versions of this list
at each AssignStm.
*)

exception NoVariableFound 		  

fun lookup (table: (string * int) list, v: string) =
    case table of
	[] => raise NoVariableFound
      | (x,y)::xs => if x = v then y else lookup(xs, v)

(* test *)
val tb1 = [("s", 1), ("v", 2)]
val two = lookup(tb1, "v")
						

fun interpStm (stm_x, table) =
    case stm_x of
	CompoundStm (x,y) =>
	let
	    val table' = interpStm(x, table)
	in
	    interpStm(y, table')
	end
      | AssignStm (i, exp_x) =>
	let
	    val (x', t') = interpExp(exp_x, table)
	in
	    (i, x') :: t'
	end

      | PrintStm exps =>
	let
	    fun helper (x, t) =
		case x of
		    [] => (print "\n"; t)
		   |x'::xs'  =>
		    let
			val (x'', t'') = interpExp (x', t)
		    in
			print ((Int.toString x'') ^ " ");
			helper(xs', t'')
		    end
	in
	    helper (exps, table)
	end

(* (id * int list) -> int  *)	    
and interpExp (exp_x: exp, table: (string * int) list) =
    case exp_x of
	IdExp v => (lookup (table, v), table)
      | NumExp i => (i, table)
      | OpExp (e1, oper, e2) =>
	let
	    val (v1, t1) = interpExp(e1, table)
	    val (v2, t2) = interpExp(e2, t1)
	in
	    case oper of
		Plus => ((v1 + v2), t2)
	      | Minus => ((v1 - v2), t2)
	      | Times => ((v1 * v2), t2)
	      | Div => ((v1 div v2), t2)
	end
      | EseqExp (st, e) => (interpStm(st, table); interpExp(e, table))

							  
fun interp (expr: stm) =
    (interpStm(expr, []); ())
