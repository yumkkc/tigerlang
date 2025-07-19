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
                emit(A.OPER {assem = "sw `s1 0(`s0)\n", 
                            src = [munchExp e1, munchExp e2],
                            dst=[], jump=NONE
                            }) 

            | munchStm (T.MOVE (T.TEMP t, T.CALL (T.NAME lab, args))) = (
                    emit (A.OPER {
                        assem = "jal `j0\n",
                        src = munchArgs 0 args,
                        dst=Translate.calldefs, jump=SOME [lab]
                    });
                    emit (A.MOVE {
                        assem = "move `d0 `s0 \n",
                        src = Frame.RV, dst=t
                    })
            )
            | munchStm (T.MOVE (d, s)) = 
                emit(A.MOVE {
                    assem="move `d0 `s0\n", 
                    src=munchExp s,
                    dst=munchExp d
                })
            | munchStm (T.JUMP ((T.NAME _), lablist)) = 
                emit (A.OPER {
                    assem = "j `j0\n", src = [], dst = [],
                    jump = SOME lablist })
            | munchStm (T.JUMP (e0, _)) = 
                emit(A.OPER {
                    assem = "jr `d0", 
                    src=[], dst=[munchExp e0], jump=NONE
                })
            | munchStm (T.CJUMP (T.EQ, e1, e2, l1, _)) = 
                emit (A.OPER {
                    assem = "beq `s0 `s1 `j0\n",
                    src=[munchExp e1, munchExp e2], dst=[],
                    jump=SOME [l1] })
            | munchStm (T.CJUMP (T.NE, e1, e2, l1, _)) = 
                emit(A.OPER {
                    assem = "bne `s0 `s1 `j0\n",
                    src = [munchExp e1, munchExp e2], dst=[],
                    jump=SOME [l1]
                })
            | munchStm (T.CJUMP (T.LT, e1, e2, l1, _)) = 
                emit(A.OPER {
                    assem = "blt `s0 `s1 `j0\n",
                    src = [munchExp e1, munchExp e2], dst=[],
                    jump=SOME [l1]
                })
            | munchStm (T.CJUMP (T.LE, e1, e2, l1, _)) = 
                emit(A.OPER {
                    assem = "ble `s0 `s1 `j0\n",
                    src = [munchExp e1, munchExp e2], dst=[],
                    jump=SOME [l1]
                })
            | munchStm (T.CJUMP (T.GT, e1, e2, l1, _)) = 
                emit(A.OPER {
                    assem = "bgt `s0 `s1 `j0\n",
                    src = [munchExp e1, munchExp e2], dst=[],
                    jump=SOME [l1]
                })
            | munchStm (T.CJUMP (T.GE, e1, e2, l1, _)) = 
                emit(A.OPER {
                    assem = "bge `s0 `s1 `j0\n",
                    src = [munchExp e1, munchExp e2], dst=[],
                    jump=SOME [l1]
                })
            | munchStm (T.LABEL lab) = emit(A.LABEL {assem=Symbol.name lab ^ ":\n", lab=lab})

            | munchStm (T.EXP (T.CALL (e1, args))) = 
                emit (A.OPER {
                    assem = "jal `s0\n",
                    src = (munchExp e1) :: (munchArgs 0 args), 
                    dst=Translate.calldefs, jump=NONE

                })
            | munchStm (T.EXP exp) = let val _ = munchExp exp in () end
            | munchStm _ = ErrorMsg.impossible "Not matching in code generation"

        and

        munchExp (T.TEMP t) = 
        (
            if t = Frame.FP then
                result(fn r => 
                    emit(A.OPER{
                        assem="addiu `d0, `s0, " ^ Symbol.name (Frame.name frame) ^ "_framesize\n",
                        src=[Frame.SP], dst=[r], jump=NONE
                    })
                )
            else 
                t
        )
        | munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) = 
            result(fn r => emit (
                A.OPER {
                    assem="lw `d0 " ^ Int.toString i ^ "(`s1)\n",
                    src=[munchExp e1], dst=[r], jump=NONE
                }))
        | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) = 
            result(fn r => emit (
                A.OPER {
                    assem="lw `d0 " ^ Int.toString i ^ "(`s1)\n",
                    src=[munchExp e1], dst=[r], jump=NONE
                }))
        | munchExp (T.MEM(T.CONST i)) = 
            result(fn r => emit (
                A.OPER {
                    assem = "lw `d0 " ^ Int.toString i ^ "($r0)\n",
                    src=[], dst=[r], jump=NONE
                }))
        | munchExp (T.MEM e1) = 
            result(fn r => emit (
                A.OPER {
                    assem="lw `d0 `s0\n",
                    src=[munchExp e1], dst=[r], jump=NONE
                }))
        | munchExp (T.BINOP (T.PLUS, e1, e2)) = 
            result (fn r => emit (
                A.OPER {
                    assem = "add `d0 `s0 `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }
            ))
        | munchExp (T.BINOP (T.MINUS, e1, e2)) = 
            result (fn r => emit (
                A.OPER {
                    assem = "sub `d0 `s0 `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }
            ))
        | munchExp (T.BINOP (T.MUL, e1, e2)) = 
            result (fn r => emit (
                A.OPER {
                    assem = "sll `d0 `s0 `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }
            ))
        | munchExp (T.BINOP (T.DIV, e1, e2)) = 
            result (fn r => emit (
                A.OPER {
                    assem = "srl `d0 `s0 `s1\n",
                    src=[munchExp e1, munchExp e2], dst=[r], jump=NONE
                }
            ))
        | munchExp (T.CONST i) = 
            result (fn r => emit (
                A.OPER {
                    assem = "addi `d0 $r0 " ^ Int.toString i ^ "\n",
                    src=[], dst=[r], jump=NONE
                }))
        | munchExp (T.NAME lab) = 
            result (fn r => emit (
                A.OPER {
                    assem = "la `d0, " ^ Symbol.name lab ^ "\n",
                    src = [], dst = [r], jump=NONE
                }
            ))
        | munchExp _ = ErrorMsg.impossible "Not matching in code generation"

        and
        munchArgs index args = 
            let val regs = Translate.getArgReg()
                fun munchArg i (arg::args') = 
                    let val arg_assem = munchExp arg
                        val reg = List.nth (regs, i)
                    in
                        if (i <= 3) then
                            (munchStm (T.MOVE(T.TEMP reg, T.TEMP arg_assem))) :: munchArg (i + 1) args'
                        else 
                            (munchStm (T.MOVE (T.BINOP (T.PLUS, 
                                T.CONST ((i-4)*Translate.wordsize), T.TEMP Translate.SP), T.TEMP arg_assem))) 
                                    :: munchArg (i + 1) args'
                    end

                | munchArg _ [] = []
            in
                munchArg index args;
                regs
            end

    in
        munchStm stm;
        !ilist
    end
end