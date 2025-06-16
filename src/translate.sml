signature TRANSLATE =
sig
    type level
    type access
    type exp

    val outermost : level
    val newLevel : {parent : level, name: Temp.label,
                   formals: bool list} -> level
    val allocLocal: level -> bool -> access
    val formals: level -> access list

end

structure Translate : TRANSLATE =
struct

structure Frame = MipsFrame

datatype level = innerlevel of {parent : level,
                                name: Temp.label,
                                frame: Frame.frame } | outermost

type access = level * Frame.access
type exp = unit

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
  | allocLocal  outermost _ = raise TranslationError ("Cannot assign local variable in outermost env")

fun formals level : access list =
    case level of
        innerlevel {parent=_, name=_, frame=frm} => let val _::main_access = Frame.formals frm
                                                        val trans_access = map (fn a => (level, a)) main_access
                                                         in trans_access
                                                    end
      | outermost =>  raise TranslationError ("No formals present in outermost layear")
end
