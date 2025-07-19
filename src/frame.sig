signature FRAME =
sig
    type frame
    type access
    type register
    val newFrame : {name : Temp.label,
                   formals: bool list} -> frame

    datatype frag = PROC of {body : Tree.stm, frame: frame}
                    | STRING of Temp.label * string

    

    val name : frame -> Temp.label
    val formals: frame -> access list
    val allocLocal : frame -> bool -> access

    (*important variables*)
    val FP: Temp.temp
    val wordSize: int
    val RV : Temp.temp
    val SP : Temp.temp
    (* expression for IR *)
    val exp : access -> Tree.exp -> Tree.exp
    val externalCall: string -> Tree.exp list -> Tree.exp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm (* implement some view shift here *)
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list -> { prolog : string, body : Assem.instr list, epilog : string }
    val tempMap: register Temp.Table.table
    val args_reg_list : Temp.temp list
    val calldefs : Temp.temp list

    val get_reg_names : Temp.temp -> string
end
