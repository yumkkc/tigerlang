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
    val assignVar : exp -> exp -> exp
    val initVar : access -> level -> exp -> exp
    val arthExpr : exp -> exp -> Absyn.oper -> exp
    val intExp : level -> int -> exp
    val callExp : Temp.label -> level -> level -> exp list -> exp
    val initArray : level -> exp -> exp -> exp
    val recordInit : level -> exp list -> exp
end


structure Translate : TRANSLATE =
struct

structure Frame = MipsFrame
structure T = Tree

datatype level = innerlevel of {parent : level,
                                name: Temp.label,
                                frame: Frame.frame,
                                unique: unit ref
                                } | outermost

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
     frame= Frame.newFrame{name=Temp.newlabel(), formals=true::formals},
     unique = ref ()
    }

exception TranslationError of string

fun allocLocal (innerlevel level) isescape :access =
    let
        val {parent=_, name=_, frame=frame, ...} = level
        val access = Frame.allocLocal frame isescape
    in
        (innerlevel level, access)
    end
  | allocLocal  outermost _ = raise TranslationError "Cannot assign local variable in outermost env"

fun formals level : access list =
    case level of
        innerlevel {parent=_, name=_, frame=frm, ...} => let val _::main_access = Frame.formals frm
                                                        val trans_access = map (fn a => (level, a)) main_access
                                                         in trans_access
                                                    end
      | outermost =>  raise TranslationError "No formals present in outermost layer"

(* main functions *)

fun staticLinks (innerlevel destination : level) (innerlevel current: level) : Tree.exp = 
    if (#unique destination) = (#unique current) 
    then Tree.TEMP Frame.FP
    else
        let
            val sl::_ = Frame.formals (#frame current)
        in
            Frame.exp sl (staticLinks (innerlevel destination) (#parent current))
        end
    | staticLinks _ _ = (ErrorMsg.impossible "Cannot do anything in outerlevel")


fun simpleVar (var_access: access, curr_level: level) =
    let
        val (declr_level, frame_access) = var_access
        val var_staticlink = staticLinks declr_level curr_level
    in
        Ex (Frame.exp frame_access var_staticlink)
    end

fun assignVar l_var value : exp =
        Nx(
            T.MOVE (
                UnEx l_var,
                UnEx value
            )
        )

fun initVar (_, dec_access) current_level init = 
    let
        val l_expr = Frame.exp dec_access (T.TEMP Frame.FP)
    in
        Nx (
            T.MOVE (
                l_expr,
                (UnEx init)
            )
        )
    end


fun arthExpr exp1 exp2 (oper:Absyn.oper) =
    let
        val t_exp1 = UnEx exp1
        val t_exp2 = UnEx exp2
        fun make_bin_op t_op = Ex (T.BINOP (t_op, t_exp1, t_exp2))
        fun make_jump_op t_op = Cx (fn (t:Temp.label, f:Temp.label) => T.CJUMP (t_op, t_exp1, t_exp2, t, f))

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

fun intExp (curr_level : level) int_exp = Ex (T.CONST int_exp)

fun callExp label curr_level func_level exp_list = 
    let
        val fun_static_link = staticLinks func_level curr_level
        val tree_exp_list = map UnEx exp_list
    in
        Ex (T.CALL (T.NAME label,
                fun_static_link :: tree_exp_list
                )
            )
    end

fun externalCall name exps  = 
    let
      val tree_exp_list = map UnEx exps
    in
      Frame.externalCall name tree_exp_list
    end

fun initArray curr_level size init = 
    let
        val temp' = Temp.newtemp()
        val args = size :: init :: (intExp curr_level Frame.wordSize) :: []
    in
    Ex(
        T.ESEQ (
            T.MOVE (
                T.TEMP temp',
                externalCall "initArray" args
            ),
            T.TEMP temp'            
        )
    )
    end

fun recordInit curr_level (exp_list: exp list) = 
    let
        val expr_len = List.length exp_list
        val size_list = T.CONST (expr_len * Frame.wordSize) :: []
        val record_add = Frame.externalCall "initRecord" size_list
        val res_temp = Temp.newtemp()
        val tree_exp_list = map UnEx exp_list

        fun generate_move_stm index exp =
                        T.MOVE (
                    T.BINOP (
                        T.PLUS,
                        T.MEM (T.TEMP res_temp),
                        T.BINOP(
                            T.MUL,
                            T.CONST index,
                            T.CONST Frame.wordSize
                        )
                    ),    
                    exp)

        fun moveRecValue [] _= ErrorMsg.impossible "Cannot have record without any entries"

        | moveRecValue (exp::[]) index =  generate_move_stm index exp

        | moveRecValue (exp::exps) index = 
            T.SEQ (
                (generate_move_stm index exp),
                moveRecValue exps (index+1)
            )
    in
    Ex(
        T.ESEQ(
            T.SEQ (
                T.MOVE (T.TEMP res_temp, record_add),
                (moveRecValue tree_exp_list expr_len)
            ),
            T.TEMP res_temp
        )
    )
    end

end
