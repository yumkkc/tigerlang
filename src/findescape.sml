structure FindEscape : sig val findEscape : Absyn.exp -> unit
                       end =
struct
type depth = int
type escEnv = (depth * bool ref) Symbol.table
structure A = Absyn
fun traverseExp (env: escEnv, d: depth, s:Absyn.exp): unit =
    let
        fun trExp (A.VarExp var) = traverseVar (env, d, var)
          | trExp (A.CallExp {func, args, pos}) = app trExp args
          | trExp (A.OpExp {left, oper, right, pos}) = (
            trExp left;
            trExp right
        )
          | trExp (A.RecordExp {fields, typ, pos}) =
            let
                val exp_list = map (fn (_, exp, _) => exp) fields
            in
                app trExp exp_list
            end
          | trExp (A.SeqExp exps) =
            let
                val exp_list = map (fn (exp, _) => exp) exps
            in
                app trExp exp_list
            end
          | trExp (A.AssignExp {var, exp, pos}) = trExp exp
          | trExp (A.IfExp {test, then', else'=NONE, pos}) = (trExp test; trExp then')
          | trExp (A.IfExp {test, then', else'=SOME elsexp, pos}) = (trExp test; trExp then'; trExp elsexp)
          | trExp (A.WhileExp {test, body, pos}) = (trExp test; trExp body)
          | trExp (A.ForExp {var, escape, lo, hi, body, pos}) =
            let
                val _ = (escape := false)
                val env' = Symbol.enter (env,var, (d, escape))
            in
                (trExp lo;
                 trExp hi;
                 traverseExp (env', d, body)
                )
            end
          | trExp (A.ArrayExp {typ, size, init, pos}) = (trExp size; trExp init)
          | trExp (A.LetExp {decs, body, pos}) =
            let
                val env' = traverseDecs (env, d, decs)
            in
                traverseExp (env', d, body)
            end
          | trExp _ = ()
    in
        trExp s
    end
and traverseVar (env: escEnv, d: depth, s:Absyn.var): unit =
    let
        fun trVar (A.SimpleVar(symbol, pos)) =
            let
                val sym_res = Symbol.look(env, symbol)
            in
                case sym_res of
                    SOME (d', escape) => if (d > d') then escape := true else ()
                  | NONE => (ErrorMsg.error pos ("Error while finding escape " ^ Symbol.name symbol))

            end

          | trVar (A.FieldVar (var, _, _)) =
            trVar var
          | trVar (A.SubscriptVar (var, _, _)) =
            trVar var
    in
        trVar s
    end
and traverseDecs (env, d, decs: Absyn.dec list): escEnv =
    let
        fun trDec ((A.VarDec {name, escape, typ, init, pos}), env) = (
                escape := false;
                Symbol.enter(env, name, (d, escape)))


          | trDec ((A.FunctionDec fundecs), env) =
            let
                fun process_fundec {name, params, result, body, pos} =
                    let
                        fun process_params ({name, escape, typ, pos}, env') =
                            (escape := false;
                             Symbol.enter (env',  name ,(d, escape))
                            )
                        val env' = foldl process_params env params
                        val d' = d + 1
                    in
                        traverseExp (env', d', body)
                    end
            in
                app process_fundec fundecs;
                env
            end

          | trDec (_,env) = env
    in
        foldl trDec env decs
    end
fun findEscape (prog: Absyn.exp) : unit =
    let
        val env: escEnv = Symbol.empty
        val d: int = 1
    in
        traverseExp (env, d, prog)
    end
end
