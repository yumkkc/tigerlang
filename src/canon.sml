signature CANON = 
sig
    val linearize: Tree.stm -> Tree.stm list
    (*val basicBlock : Tree.stm list -> (Tree.stm list list * Temp.label)*)
    (* val traceSchedule : Tree.stm list list * Temp.label -> Tree.stm list *)
end

structure Canon :> CANON =  struct

structure T = Tree

val nop = T.EXP(T.CONST 0)

fun linearize cur_stm = 
let
    infix %
    fun (T.EXP(T.CONST _)) % x = x
        | x % (T.EXP(T.CONST _)) = x
        | x % y = T.SEQ (x, y)

    fun commute (_, T.CONST _) = true
    | commute (_, T.NAME _) = true
    | commute (T.EXP(T.CONST _), _) = true
    | commute _ = false

    fun reorder (T.CALL (f,args)::exps) =  
                        let
                            val new_temp = Temp.newtemp()
                            val new_exp = T.ESEQ (
                                T.MOVE(T.TEMP new_temp, T.CALL (f,args)),
                                T.TEMP new_temp
                            )
                        in
                            reorder (new_exp::exps)
                        end
        | reorder (exp::exps) = 
        let
            val (s1, e1) = do_exp exp
            val (sr, es)  = reorder exps
        in
            if commute(sr, e1) then
            (s1 % sr, e1::es)
            else
                let
                    val new_temp = Temp.newtemp()
                    val move_exp = T.MOVE(T.TEMP new_temp, e1)
                in 
                    (s1 % move_exp % sr, (T.TEMP new_temp) :: es)
                end
        end
        | reorder [] = (nop, nil)


    and reorder_stm (exps, f) = 
        let
            val (stm1, new_exps) = reorder exps
            val stm2 = f new_exps
        in
            stm1 % stm2
        end

    and reorder_exp (exps, f) = 
        let
            val (stm, new_exps) = reorder exps
            val new_exp = f new_exps
        in
            (stm, new_exp)
        end

    and do_stm (T.JUMP (e, labs)) =  reorder_stm ([e], fn [e] => T.JUMP (e, labs))
        | do_stm (T.LABEL lab) = (T.LABEL lab)
        | do_stm (T.MOVE (T.TEMP temp, T.CALL (f, args))) = reorder_stm ((f :: args),
                                                        fn (f::args) => T.MOVE (T.TEMP temp, T.CALL (f, args))
                                                        )
        | do_stm (T.MOVE (T.TEMP t, b))  = reorder_stm ([b], fn[b]=> T.MOVE(T.TEMP t, b))
        | do_stm (T.MOVE (exp1, exp2)) =  reorder_stm ([exp1, exp2], 
                                                    fn [exp1, exp2] => (T.MOVE (exp1, exp2))
                                                    )
        | do_stm (T.SEQ (stm1, stm2)) = (do_stm stm1) % (do_stm stm2)
        | do_stm (T.CJUMP (re, a, b, lab1, lab2)) = 
                reorder_stm ([a, b], fn [a,b] => T.CJUMP (re,a,b,lab1,lab2))
        | do_stm (T.EXP (T.CALL (f, args))) = reorder_stm ((f::args), fn (f::args) => T.EXP (T.CALL (f, args)))
        | do_stm (T.EXP e) = reorder_stm ([e], fn [e] => (T.EXP e))


    and do_exp (T.BINOP (p, a, b))   = 
                reorder_exp ([a, b], fn[a,b] => T.BINOP (p, a, b))
        | do_exp (T.MEM exp) = reorder_exp ([exp], fn [exp] => (T.MEM exp))
        | do_exp (T.ESEQ (stm1, exp1)) = let
                                            val (stm2, exp') = reorder_exp ([exp1], fn [exp1] => exp1)
                                            val stm1' = do_stm stm1
                                        in  
                                            (stm1' % stm2, exp')
                                        end
        | do_exp (T.CALL args) = reorder_exp ([(T.CALL args)], fn [exp] => exp)
        | do_exp rest_exp = reorder_exp ([], fn [] => rest_exp)


    and linear ((T.SEQ (a, b)), lin_stm) =  linear(a, linear(b,lin_stm))    
        | linear (lst, lin_stm) = lst :: lin_stm

    val stm' = do_stm cur_stm

in
          linear (stm', nil)
end
end
