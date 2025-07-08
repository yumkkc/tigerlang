structure Main =
struct
structure Frame = MipsFrame
fun main filename =
    let
        val ast = Parse.parse filename
        val _ = FindEscape.findEscape ast
        val frag_lists = Semant.transProg ast
        fun print_frag [] = ()
            | print_frag (frag::frags) =
                let
                    val Frame.PROC o_frag = frag
                in
                    Printtree.printtree (TextIO.stdOut, (#body o_frag));
                    print("-----------------------\n\n");
                    print_frag frags
                end
    in
        print_frag frag_lists;
        frag_lists
    end
end
