structure codegentest = struct
structure T = Tree
val test1 = Temp.newtemp()
val test2 = Temp.newtemp()
val frm = MipsFrame.newFrame {formals=[true], name = Temp.newlabel()}
val test = T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST 0, T.TEMP test1)), T.TEMP test2)
val new_lab = Temp.newlabel()
val new_lab2 = Temp.newlabel()
val test2 = T.CJUMP (T.EQ, T.TEMP test1, T.TEMP test1, new_lab, new_lab2)
val res = MipsCodeGen.codegen frm test2;
val res = hd res
fun get_codegen () = test2
fun test () = Assem.format Temp.makestring res
end
