structure MipsFrame : FRAME = struct

datatype access = InFrame of int | InReg of Temp.temp

type frame = { name: Temp.label, formals: (access * bool) list, locals: (access * bool) list ref }

fun assignMem() = InFrame 0 (* TODO: Change later *)

fun assignReg() = InReg (Temp.newtemp())

fun assignMemOrReg true = (assignMem(), true)
  | assignMemOrReg false = (assignReg(), false)

fun newFrame {name, formals}: frame = {name = name,
                                formals = (map assignMemOrReg formals),
                                locals = ref []}

fun name {name=name, formals=_, locals=_ } = name

fun formals {name=name, formals=formals, locals=_} =
    (map (fn (access, _) => access) formals)

fun allocLocal {name, formals, locals} isescape =
    let
        val new_access = assignMemOrReg isescape
    in
        locals := new_access :: !locals;
        #1 new_access
    end

    
end
