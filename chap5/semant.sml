structure Translate = struct type exp = unit end

signature SEMANT =
sig
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    datatype context = LOOP | NOTLOOP

    type expty = {exp: Translate.exp, ty: Types.ty}

    val transExp: venv * tenv * Absyn.exp * context  -> expty
(*    val transVar: venv * tenv * Abysn.dec -> expty *)
    val transDecs: venv * tenv * Absyn.dec list -> {venv: venv, tenv: tenv}
    val transTy : tenv * Absyn.ty -> Types.ty
    val transProg: Absyn.exp -> expty
end

structure Semant : SEMANT =
struct

structure E = Env
structure A = Absyn

type expty = {exp: Translate.exp, ty: Types.ty}

type venv = E.enventry Symbol.table
type tenv = Types.ty Symbol.table



fun checkInt ({exp, ty}, pos) = if ty = Types.INT then true else
                                ((ErrorMsg.error pos ("Integer Expected"));
                                 false)

fun checkUnit ({exp = _, ty=Types.UNIT}, pos) = ()
  | checkUnit (_, pos) = ErrorMsg.error pos ("unit required")

fun actual_ty (Types.NAME (s, tyref)) = (case !tyref of
                                      SOME t => actual_ty t
                                    | NONE => Types.NIL)
  | actual_ty t = t

fun find_type (tenv, sym, pos) =
    let
        val ty = Symbol.look (tenv, sym)
    in case ty of
           SOME t => t
         | NONE => (
             (ErrorMsg.error pos ("Unknown type " ^ Symbol.name sym));
                 Types.NIL)
    end

fun is_list_size_eq (a, b) = length a = length b

fun check_type_equality (ty1: Types.ty, ty2: Types.ty, pos, errormsg) =
    case (actual_ty ty1 = actual_ty ty2) of
        true => ()
      | false =>  (ErrorMsg.error pos errormsg)


fun enterparam ({name, ty}, venv) =
                    Symbol.enter (venv, name,
                        Env.VarEntry {ty=ty})

datatype context = LOOP | NOTLOOP


(* Absyn.exp *)
fun transExp (venv, tenv, exp, context: context) =
    let

        fun check_arth_param (left, right, pos) =
            (checkInt (trexp left, pos);
                     checkInt (trexp right, pos);
                     {exp = (), ty=Types.INT} )
        and

        check_eq_noteq_param (left, right, pos) =
            let
                val {exp = _, ty = ltype} = trexp left
                val {exp = _, ty = rtype} = trexp right
            in
                case (ltype, rtype) of
                    (Types.INT, Types.INT) => {exp = (), ty=Types.INT}
                    | (Types.RECORD _, Types.RECORD _) => {exp = (), ty=Types.INT}
                    | (Types.ARRAY _, Types.ARRAY _) => {exp = (), ty=Types.INT}
                    | (_, _) => ((ErrorMsg.error pos ("Cannot compare the two expression"));
                                                    {exp = (), ty=Types.NIL})
            end

        and

        find_ty (sym1, []) = NONE
        | find_ty (sym1, (sym2, typ2)::rest) =
          if (sym1 = sym2) then SOME typ2
          else find_ty (sym1, rest)

        and

         trexp (A.NilExp) = {exp = (), ty = Types.UNIT}

         | trexp (A.OpExp {left, oper=A.PlusOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.MinusOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.TimesOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.DivideOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.LtOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.LeOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.GtOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=A.GeOp, right, pos}) =
            check_arth_param (left, right, pos)

          | trexp  (A.OpExp {left, oper=_, right, pos}) =
            check_eq_noteq_param (left, right, pos)

          | trexp (A.VarExp var) = trvar var

          | trexp (A.IntExp arg) = {exp = (), ty = Types.INT}

          | trexp  (A.StringExp arg) = {exp = (), ty = Types.STRING}

          | trexp (A.LetExp {decs, body, pos}) =
            (
              let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
              in transExp (venv', tenv', body, NOTLOOP)
              end
            )

          | trexp (A.BreakExp pos) = (case context of
                                     NOTLOOP => ((ErrorMsg.error pos "break statment not allowed here"); {exp=(), ty=Types.UNIT})
                                   | LOOP => {exp =(), ty=Types.UNIT}
                                 )

          | trexp (A.RecordExp {fields, typ, pos}) = (
            case Symbol.look (tenv, typ) of
                SOME (Types.RECORD (record_typs, _ )) =>
                let
                    fun loop [] = {exp = (), ty = Types.RECORD (record_typs, ref ())}
                      | loop ((symbol, exp, pos)::rest) = (
                        case find_ty (symbol, record_typs) of
                            SOME(ty) => let val {exp = _, ty=expty} = trexp exp
                                        in
                                            if (expty = ty ) then loop rest
                                            else ((ErrorMsg.error pos ("Type of record " ^ Symbol.name typ ^ " does not match" ));
                                                      {exp=(), ty=Types.NIL})
                                        end
                          | NONE => ((ErrorMsg.error pos (Symbol.name symbol ^ " not found in record: " ^ Symbol.name typ));
                                         {exp = (), ty=Types.NIL})
                    )

                in
                    loop fields
                end
              | _ => ((ErrorMsg.error pos ("No record of type " ^ Symbol.name typ ^ " found"));
                             {exp = (), ty=Types.NIL})
          )

          | trexp (A.ArrayExp {typ, size, init, pos}) = (
            case (Symbol.look (tenv, typ)) of
                SOME (Types.ARRAY (arr_ty, _)) => (
                case trexp exp of
                    {exp = _, ty=Types.INT} => (
                              let val {exp = (), ty = init_ty} = trexp init
                              in
                                  if (init_ty = arr_ty)
                                  then {exp = (), ty=(Types.ARRAY (arr_ty, ref ()))}
                                  else ((ErrorMsg.error pos (Symbol.name typ ^ " does not match the type of initialization"));
                                            {exp = (), ty=Types.NIL})
                              end )
                            | _ => ((ErrorMsg.error pos ("size should be always of integer type"));
                                        {exp = (), ty = Types.NIL})
            )
              | _ => ((ErrorMsg.error pos (Symbol.name typ ^ " should be of array type");
                                      {exp = (), ty=Types.NIL}))
          )

          | trexp  (A.AssignExp {var, exp, pos}) = (
              let
                  val {exp=_, ty=var_ty} = trvar var
                  val {exp=_, ty=exp_ty} = trexp exp
            in
                if (var_ty = exp_ty) then {exp=(), ty = var_ty} else
                ((ErrorMsg.error pos ("type is assign statement does not match"));
                     {exp=(), ty = Types.NIL})
            end
          )

          | trexp (A.SeqExp exps) =
            let fun loop [] = {exp = (), ty = Types.UNIT}
                  | loop ((exp, _)::[]) = trexp exp
                  | loop ((exp, _)::exps) = ((trexp exp);
                                        loop exps )
            in
                loop exps
            end

          | trexp  (A.CallExp {func, args, pos}) =
            (
              case Symbol.look (venv, func) of
                  SOME (Env.FunEntry {formals, result}) => (
                   let
                       fun check_type_param (formal::formals, arg::args) =
                           let val {exp=_, ty=t'} = trexp arg
                           in if not (actual_ty formal = actual_ty t') then
                                  ((ErrorMsg.error pos "type does not match");
                                       false andalso check_type_param (formals, args))
                              else true andalso check_type_param (formals, args)
                           end
                         | check_type_param  ([], []) = true
                         | check_type_param (_, _) = (ErrorMsg.error pos (Symbol.name func ^ " length of parameters does not match the ones passed");
                          false)
                       in
                           check_type_param (formals, args);
                                            {exp = (), ty=result}
                       end
               )
               |  _ => ((ErrorMsg.error pos (Symbol.name func ^ " not a function variable"));
                            {exp = (), ty=Types.NIL})
            )

          | trexp (A.IfExp {test, then', else'=SOME(els_exp), pos}) =
            let
                val testA = trexp test
                val {exp = _, ty = thenA} = trexp test
                val {exp = _, ty = elseA} = trexp els_exp
            in
                checkInt(testA, pos);
                check_type_equality(thenA, elseA, pos, ("then and else should have same type"));
                {exp = (), ty = elseA}
            end

          | trexp (A.IfExp {test, then', else'= NONE, pos}) =
            let
                val testA = trexp test
                val thenA = trexp test
                val {exp = _, ty=then_type} = thenA
            in
                checkInt(testA, pos);
                checkUnit(thenA, pos);
               {exp = (), ty = Types.UNIT}
            end

          | trexp (A.WhileExp {test, body, pos}) =
            let val test_ty = trexp test
                val body_ty = transExp (venv, tenv, body, LOOP)
            in
                checkInt (test_ty, pos);
                checkUnit (body_ty, pos);
                {exp=(), ty = Types.UNIT}
            end

          | trexp (A.ForExp {var, escape, lo, hi, body, pos}) =
            let
                val lo_ty = trexp lo
                val hi_ty = trexp hi
                val venv' = Symbol.enter (venv, var, (Env.VarEntry {ty=Types.INT}))
                val body_ty = transExp (venv', tenv, body, LOOP)
            in
                checkInt(lo_ty, pos);
                checkInt (hi_ty, pos);
                checkUnit (body_ty, pos);
                {exp = (), ty=Types.UNIT}
            end

        and trvar (A.SimpleVar (id, pos)) =
            (case Symbol.look(venv, id)
              of SOME(E.VarEntry{ty}) => {exp = (), ty = actual_ty ty}
               | _ => ((ErrorMsg.error pos ("undefined variable " ^ Symbol.name id));
                       {exp = (), ty=Types.NIL}))

          | trvar (A.SubscriptVar (var, exp, pos)) = (
            case (trvar var) of
                {exp=_, ty = Types.ARRAY (var_ty, _)} => (
                        case (trexp exp) of
                            {exp = _, ty = Types.INT} => {exp = (), ty=var_ty}
                            | _ => ((ErrorMsg.error pos ("Array subscript type should be Integer"));
                                                          {exp = (), ty = Types.NIL})
            )
                      | _ => ((ErrorMsg.error pos ("Variable should be an array"));
                                                          {exp = (), ty = Types.NIL})
          )

          | trvar (A.FieldVar (var, sym, pos)) = (
              case (trvar var) of
                  {exp = _, ty = (Types.RECORD (sym_list, _))} =>
                            (case find_ty (sym, sym_list) of
                                 SOME rc_ty => {exp = (), ty = rc_ty}
                                                        | NONE => (ErrorMsg.error pos (Symbol.name sym ^ " not found in record");
                                                                                  {exp = (), ty = Types.INT})
                            )
                          | _ => (ErrorMsg.error pos ("Not a record type");
                                                 {exp = (), ty = Types.INT})
          )

    in
        trexp exp
    end

(* Absyn.dec *)
and transDecs (venv, tenv, dec::decs) =
    let
        fun transDec (venv, tenv, A.VarDec {name, typ=NONE, init, ...}) =
            let val {exp, ty} = transExp(venv, tenv, init, NOTLOOP)
            in {tenv = tenv, venv = Symbol.enter (venv, name, Env.VarEntry {ty=ty})}
            end

         (* TODO: some more changes are required -> if type failed to add or not *)

          | transDec (venv, tenv, A.VarDec {name, typ=SOME(sym_ty, sym_pos), pos, init, ...}) = (
            case Symbol.look (tenv, sym_ty) of
                SOME(res_ty) => (
                             let
                                 val {exp, ty} = transExp(venv, tenv, init, NOTLOOP)


                             in
                                check_type_equality(ty, res_ty, pos,
                                        ("result type of " ^ Symbol.name name ^ " and " ^ Symbol.name sym_ty ^ " does not match"));
                                 {tenv = tenv, venv = Symbol.enter (venv, name, Env.VarEntry {ty=ty})}
                             end
             )
               | NONE => ((ErrorMsg.error sym_pos (Symbol.name sym_ty ^ " type not found"));
                {venv=venv, tenv=tenv})
          )

          | transDec (venv, tenv, A.TypeDec declars) =
            let
                fun cycle_check (pos, sym, SOME(Types.NAME (sym2, tyref))) =
                    if (sym2 = sym) then ((ErrorMsg.error pos ("Illegal cycle detected in type " ^ Symbol.name sym)); true)
                    else cycle_check (pos, sym, !tyref)

                  | cycle_check _ = false


                fun iterate_decs (venv, tenv) =
                    let
                        fun iterate_dec {name, ty, pos} =
                            let
                                fun look_type (tenv, name, pos) =
                                    case Symbol.look(tenv, name) of
                                        SOME ty => ty
                                      | NONE => ((ErrorMsg.error pos ("type variable not found " ^ Symbol.name name)); Types.NIL)

                                val Types.NAME(nameRef, typRef) = look_type (tenv, name, pos)
                                val replace_ty = case ty
                                                  of A.NameTy (sym, pos) =>
                                                               if not (cycle_check (pos,name, SOME(look_type(tenv,sym,pos)))) then
                                                               Types.NAME (sym, ref (SOME(look_type(tenv, sym, pos))))
                                                               else Types.NIL
                                                   | A.ArrayTy (sym, pos) =>
                                                               Types.ARRAY (look_type(tenv, name, pos), ref ())
                                                   | A.RecordTy fields =>
                                                               Types.RECORD (map (fn ({name, escape, typ, pos}) =>
                                                                                               (name, look_type (tenv, typ, pos))) fields, ref())
                            in
                                typRef := SOME(replace_ty)
                            end
                    in
                        app iterate_dec declars
                    end
                fun initialize ({name, ty, pos}, tenv) = Symbol.enter (tenv, name, Types.NAME(name, ref NONE))
                val tenv' = foldr initialize tenv declars
            in
                iterate_decs (venv, tenv');
                {tenv=tenv', venv=venv}
            end

          | transDec (venv, tenv, A.FunctionDec fundecs) =
            let
                fun look_result_type (tenv, result) =
                    case result of
                        SOME (rt, pos) => (case Symbol.look(tenv, rt) of
                                               SOME ty => actual_ty ty
                                             | NONE => (ErrorMsg.error pos (Symbol.name rt ^ "Result type not found");
                                                        Types.UNIT)
                                          )
                      | NONE => Types.UNIT

                fun transparam tenv {name, typ, pos, escape} =
                    case Symbol.look(tenv, typ) of
                        SOME t => {name=name, ty=t}
                      | NONE => ((ErrorMsg.error pos (Symbol.name typ ^ " type not found"));
                                 {name=name, ty=Types.NIL})

                fun updateBodys(venv, tenv) =
                    let
                        fun enterparam ({name, ty}, venv) =
                            Symbol.enter (venv, name,
                                          Env.VarEntry {ty=ty})

                        fun updateBody({name, params, body, pos, result}) =
                            let
                                val result_ty = look_result_type (tenv, result)
                                val params' = map (transparam tenv) params
                                val venv'' = foldl enterparam venv params'
                                val {exp=_, ty=bodyty} = transExp (venv'', tenv, body, NOTLOOP)
                            in
                                (check_type_equality (result_ty, bodyty, pos, (Symbol.name name ^ " function result type does not match return of expression")))
                            end
                    in
                        app updateBody fundecs
                    end
                (* initial function headers insertion -> {venv', tenv'}*)
                fun enterFunctionHeaders({name, params, body, pos, result}, {venv, tenv}) =
                    let
                        val result_ty = look_result_type (tenv, result)
                        val params' = map (transparam tenv) params
                        val venv' = Symbol.enter (venv, name, E.FunEntry {formals = map #ty params', result=result_ty})
                    in
                        {venv=venv', tenv=tenv}
                    end
                val {venv=venv', tenv=tenv'} = foldl enterFunctionHeaders {venv=venv,tenv=tenv} fundecs
            in
                updateBodys(venv', tenv');
                {venv=venv', tenv=tenv'}
            end

        val {venv=venv', tenv=tenv'} = transDec (venv, tenv, dec)
    in
        transDecs (venv', tenv', decs)
    end

  | transDecs  (venv, tenv, []) = {venv=venv, tenv=tenv}

(* Absyn.ty *)
and transTy (tenv, ty) =
    let
        fun trTy (A.NameTy (sym, pos)) = find_type (tenv, sym, pos)

          | trTy (A.RecordTy fields)  =
            let
                fun loop ({name, typ, pos, escape}::rest) =
                    (name, find_type (tenv, typ, pos)) :: loop rest
                  | loop [] = []
                val fields' = loop fields
            in
                Types.RECORD (fields', ref ())
            end

          | trTy  (A.ArrayTy (sym, pos)) = Types.ARRAY (find_type(tenv, sym, pos), ref ())
    in
        trTy ty
    end

and transProg exp = transExp (Env.base_venv, Env.base_tenv, exp, NOTLOOP)
end
