signature FRAME =
sig
    type frame
    type access
    val newFrame : {name : Temp.label,
                   formals: bool list} -> frame

    datatype frag = PROC of {body : Tree.stm, frame: frame}
                    | STRING of Temp.label * string

    val name : frame -> Temp.label
    val formals: frame -> access list
    val allocLocal : frame -> bool -> access
    val assignMemOrReg : bool -> access * bool

    (*important variables*)
    val FP: Temp.temp
    val wordSize: int
    val RV : Temp.temp
    (* expression for IR *)
    val exp : access -> Tree.exp -> Tree.exp
    val externalCall: string -> Tree.exp list -> Tree.exp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm (* implement some view shift here *)
end
