structure Main =
struct
structure Frame = MipsFrame
fun main filename =
    let
        val ast = Parse.parse filename
        val _ = FindEscape.findEscape ast
        val frag_lists = Semant.transProg ast
        fun print_linear (exp::exps) = 
            ((Printtree.printtree(TextIO.stdOut, exp));
                    print(" ===> ");
            print_linear exps)
            | print_linear [] = print("\n--------------------------------------------\n")

        fun print_blocks (block::blocks) = ((print_linear block);
                                                                        (print "next block\n");
                                                                        print_blocks blocks)
        |  print_blocks [] = print("finish priting blocks\n\n")

        fun codegen_trace [] _ = print("finish\n")
        | codegen_trace (stm::stms) frag_frame = 
            let val res = MipsCodeGen.codegen frag_frame stm
            in List.app (fn instr => print (Assem.format Frame.get_reg_names instr)) (List.rev res); 
            codegen_trace stms frag_frame
            end

        fun print_frag [] = ()
            | print_frag (frag::frags) =
                let
                    val Frame.PROC o_frag = frag
                    val {body=frag_main, frame=frag_frame} = o_frag
                    val _ = Printtree.printtree (TextIO.stdOut, (#body o_frag))                    
                    val linearized = Canon.linearize frag_main
                    val _ = print "--------+++---------------\n\n"
                    val _ = print_linear linearized
                    val blocks = Canon.basicBlock linearized
                    val (blocks', _) = blocks
                    val _ = print "Block priting !!!!\n"    
                    val _ = print_blocks blocks'
                    val traces = Canon.traceSchedule blocks
                    val _ = print "traces printing\n\n"
                    val _ = print_linear traces
                    val _ = codegen_trace traces frag_frame
                in                    
                    print_frag frags
                end
        in
        print_frag frag_lists
    end
end
