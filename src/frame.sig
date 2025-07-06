signature FRAME =
sig
    type frame
    type access
    val newFrame : {name : Temp.label,
                   formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals: frame -> access list
    val allocLocal : frame -> bool -> access
    val assignMemOrReg : bool -> access * bool

    (*important variables*)
    val FP: Temp.temp
    val wordSize: int
    (* expression for IR *)
    val exp : access -> Tree.exp -> Tree.exp
    val arrayMem : int -> Tree.exp -> Tree.exp
    val externalCall: string -> Tree.exp list -> Tree.exp
end
