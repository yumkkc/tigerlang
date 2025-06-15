structure MipsFrame : FRAME = struct

(* determines where the value will be stored -> register (temp) or memory (int) *)
datatype access = InFrame of int | InReg of Temp.temp

(* label here is memory label which starts the location *)
type frame = { name: Temp.label,
               formals: (access * bool) list,
               locals: (access * bool) list ref
             }

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
        val (access, _) = new_access
    in
        locals := new_access :: !locals;
        access
    end

end
