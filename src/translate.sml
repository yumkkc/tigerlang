signature TRANSLATE =
sig
    type level
    type access

    datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm
             | to_be_replaced

    val outermost : level
    val newLevel : {parent : level, name: Temp.label,
                   formals: bool list} -> level
    val allocLocal: level -> bool -> access
    val formals: level -> access list

    (* manipulation *)
    val simpleVar : access * level -> exp
    val assignVar : access * exp -> exp
    val arthExpr : exp -> exp -> Absyn.oper -> exp
    val intExp : int -> exp
end


structure Translate : TRANSLATE =
struct

structure Frame = MipsFrame
structure T = Tree

datatype level = innerlevel of {parent : level,
                                name: Temp.label,
                                frame: Frame.frame } | outermost

type access = level * Frame.access

datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm
             | to_be_replaced


(* converision function exp -> Tree *)
fun UnEx (Ex e) = e
  | UnEx (Nx s)  = T.ESEQ (s, T.CONST 0)
  | UnEx (Cx genstm) = T.CONST 0
  | UnEx to_be_replaced  = T.CONST 0


fun newLevel {parent=parent, name=name, formals=formals} =
    innerlevel {parent=parent,
     name=name,
     frame= Frame.newFrame{name=Temp.newlabel(), formals=true::formals}
    }

exception TranslationError of string

fun allocLocal (innerlevel level) isescape :access =
    let
        val {parent=_, name=_, frame=frame} = level
        val access = Frame.allocLocal frame isescape
    in
        (innerlevel level, access)
    end
  | allocLocal  outermost _ = raise TranslationError "Cannot assign local variable in outermost env"

fun formals level : access list =
    case level of
        innerlevel {parent=_, name=_, frame=frm} => let val _::main_access = Frame.formals frm
                                                        val trans_access = map (fn a => (level, a)) main_access
                                                         in trans_access
                                                    end
      | outermost =>  raise TranslationError "No formals present in outermost layer"

(* main functions *)
fun simpleVar (var_access: access, curr_level: level) =
    let
        val (_, frame_access) = var_access
    in
        Ex (Frame.exp frame_access (T.TEMP Frame.FP))
    end

fun assignVar (var_access: access, value: exp) : exp =
    let
        val (_, frame_access) = var_access
    in
        Nx(
            T.MOVE (
                (Frame.exp frame_access (T.TEMP Frame.FP)),
                UnEx value
            )
        )
    end


fun arthExpr exp1 exp2 (oper:Absyn.oper) =
    let
        val t_exp1 = UnEx exp1
        val t_exp2 = UnEx exp2
        fun make_bin_op t_op = Ex (T.BINOP (t_op, t_exp1, t_exp2))
        fun make_jump_op t_op =           Cx (fn (t:Temp.label, f:Temp.label) => T.CJUMP (t_op, exp1, exp2, t, f))

        fun arExp Absyn.PlusOp = make_bin_op T.PLUS
            | arExp Absyn.MinusOp = make_bin_op T.MINUS
            | arExp Absyn.TimesOp = make_bin_op T.MUL
            | arExp Absyn.DivideOp = make_bin_op T.DIV
            | arExp Absyn.NeqOp = make_jump_op T.NE
            | arExp Absyn.LeOp = make_jump_op T.LE   
            | arExp Absyn.LtOp = make_jump_op T.LT
            | arExp Absyn.GeOp = make_jump_op T.GE
            | arExp Absyn.GtOp = make_jump_op T.GT
            | arExp Absyn.EqOp = make_jump_op T.EQ
        in
            arExp oper        
        end

fun intExp int_exp = Ex (T.CONST int_exp)
end
