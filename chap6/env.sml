signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty : ty}
           | FunEntry of {formals: ty list, result: ty}

    type tenv = ty Symbol.table
    type venv = enventry Symbol.table

    val base_tenv : tenv
    val base_venv : venv
end


structure Env : ENV =
struct
type access = unit
type ty = Types.ty

structure T = Types

datatype enventry = VarEntry of {ty : ty}
                  | FunEntry of {formals: ty list, result: ty}

type tenv = ty Symbol.table
type venv = enventry Symbol.table


val tenv_predefined = [
    ("int", T.INT),
    ("string", T.STRING)
]

fun entryenv ((id, data), env) =
    Symbol.enter (env, (Symbol.symbol id), data)

val base_tenv = List.foldl entryenv Symbol.empty tenv_predefined

val base_venv: venv = Symbol.empty

end
