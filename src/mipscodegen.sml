structure MipsCodeGen : CODEGEN = struct

structure Frame = MipsFrame
structure A = Assem
structure T = Tree

fun codegen frame (stm: Tree.stm) : Assem.instr list = 
    let val ilist = ref (nil : A.instr list)
        fun emit x = ilist := x :: !ilist
        fun result gen = let val t = Temp.newtemp() in gen t; t end

        fun munchStm(T.SEQ (a, b)) = (munchStm a; munchStm b)
            | munchStm(T.MOVE (T.MEM (T.BINOP(T.PLUS, T.CONST oft, e1)), e2)) = 
                emit (A.OPER{assem = "sw `s1 " ^ Int.toString oft  ^ "( `s0 )\n",
                            src=[munchExp e1, munchExp e2],
                            dst=[], 
                            jump=NONE})
            | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, e2)), e3)) = 
                emit(A.OPER {assem = "sw `s2 `s0(`s1)\n",
                             src = [munchExp e1, munchExp e2, munchExp e3],
                             dst = [], jump=NONE })
            | munchStm (T.MOVE (T.MEM e1, e2)) = 
                emit(A.OPER {assem = "sw `s1 0(`s0)", 
                            src = [munchExp e1, munchExp e2],
                            dst=[], jump=NONE
                            }) 
            | munchStm (T.JUMP ((T.NAME _), lablist)) = 
                emit (A.OPER {
                    assem = "j `j0", src = [], dst = [],
                    jump = SOME lablist })
            | munchStm (T.JUMP (e0, _)) = 
                emit(A.OPER {
                    assem = "jr `d0", 
                    src=[], dst=[munchExp e0], jump=NONE
                })
            | munchStm (T.LABEL lab) = emit(A.LABEL {assem=Symbol.name lab ^ ":\n", lab=lab})
            (* TODO: Procedure shoudl be here *)
            | munchStm (T.EXP exp) = let val _ = munchExp exp in () end
            | munchStm _ = ()

        and

        munchExp (T.TEMP t) = t
        | munchExp _ = result(fn x => x)
    in
        munchStm stm;
        !ilist
    end

end

