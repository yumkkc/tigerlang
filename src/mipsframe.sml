structure MipsFrame : FRAME = struct

structure T = Tree

val wordSize = 8
val FP = Temp.newtemp()
val RV = Temp.newtemp() (* return register *)

(* determines where the value will be stored -> register (temp) or memory (int) *)
datatype access = InFrame of int | InReg of Temp.temp

(* label here is memory label which starts the location *)
type frame = { name: Temp.label,
               formals: (access * bool) list,
               locals: (access * bool) list ref
             }

datatype frag = PROC of {body : Tree.stm, frame: frame}
                    | STRING of Temp.label * string

fun assignMem() = InFrame 0 (* TODO: Change later *)

fun assignReg() = InReg (Temp.newtemp())

fun assignMemOrReg true = (assignMem(), true)
  | assignMemOrReg false = (assignReg(), false)

(* first four gets assigned register *)
val limit = 4

fun assignParam [] _ = []
  | assignParam (true::formals) count = (assignMem(), true)::(assignParam formals count)
  | assignParam (false::formals) count = if (count <=limit) then (assignReg(), false)::(assignParam formals (count-1))
                                       else (assignReg(), false) :: (assignParam formals (count-1))

fun newFrame {name, formals}: frame = {name = name,
                                formals = (assignParam formals limit),
                                locals = ref []}

fun name {name=name, formals=_, locals=_ } = name

fun formals {name=_, formals=formals, locals=_} =
    (map (fn (access, _) => access) formals)

fun allocLocal {name = _, formals=_, locals} isescape =
    let
        val new_access = assignMemOrReg isescape
        val (access, _) = new_access
    in
        locals := new_access :: !locals;
        access
    end

fun exp (InFrame c) (fp: T.exp) =
    T.MEM(T.BINOP (T.PLUS, fp, T.CONST c))

  |  exp (InReg reg) (_: T.exp) = T.TEMP reg


fun externalCall (name: string) (args : Tree.exp list) = 
  T.CALL (T.NAME (Temp.namedlabel name), args)

fun procEntryExit1 (c_frame, t_stm) = t_stm (* implement view shift later *)

end
