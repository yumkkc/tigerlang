structure Test =
struct
structure T = Translate

val main_level = T.newLevel {parent=T.outermost, name=Symbol.symbol "main_level", formals=[]};
val new_access = Translate.allocLocal main_level true;
val access' = T.allocLocal main_level (true)

fun test () = T.assignVar (access', Translate.Ex (Tree. CONST 10))
end
