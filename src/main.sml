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
                    print("+++++++++++++++++++++++++\n\n");
            print_linear exps)
            | print_linear [] = print("finish\n")

        fun print_blocks (block::blocks) = ((print_linear block);
                                                                        (print "next block\n");
                                                                        print_blocks blocks)
        |  print_blocks [] = print("finish priting blocks")

        fun print_frag [] = ()
            | print_frag (frag::frags) =
                let
                    val Frame.PROC o_frag = frag
                    val frag_main = (#body o_frag)
                    val linearized = Canon.linearize frag_main
                    val (blocks, _) = Canon.basicBlock linearized
                in
                    Printtree.printtree (TextIO.stdOut, (#body o_frag));
                    print("-----------------------\n\n");
                    print_linear linearized;
                    print_frag frags;
                    print("Block priting !!!!\n");
                   print_blocks blocks

                end
        in
        print_frag frag_lists
    end
end
