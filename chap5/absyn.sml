structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

datatype var = SimpleVar of symbol * pos (* done *)
            | FieldVar of var * symbol * pos (* done *)
            | SubscriptVar of var * exp * pos (* done *)

and exp = VarExp of var (* done *)
        | NilExp (* done *)
        | IntExp of int (* done *)
        | StringExp of string * pos (* done *)
        | CallExp of {func: symbol, args: exp list, pos: pos} (* done *)
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos} (* done *)
        | RecordExp of {fields: (symbol * exp * pos) list, (* done *)
			typ: symbol, pos: pos}
        | SeqExp of (exp * pos) list (* done *)
        | AssignExp of {var: var, exp: exp, pos: pos} (* done *)
        | IfExp of {test: exp, then': exp, else': exp option, pos: pos} (* done *)
        | WhileExp of {test: exp, body: exp, pos: pos} (* done *)
	    | ForExp of {var: symbol, escape: bool ref,
		     lo: exp, hi: exp, body: exp, pos: pos} (* done *)
        | BreakExp of pos
        | LetExp of {decs: dec list, body: exp, pos: pos} (* done *)
        | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos} (* done  *)

and dec = FunctionDec of fundec list
        | VarDec of {name: symbol,
		             escape: bool ref,
		     typ: (symbol * pos) option,
		     init: exp,
		     pos: pos} (* done *)
        | TypeDec of {name: symbol, ty: ty, pos: pos} list (* done *)

and ty = NameTy of symbol * pos
       | RecordTy of field list
       | ArrayTy of symbol * pos

and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, 
		  typ: symbol, pos: pos}
   and   fundec = {name: symbol,
		   params: field list,
		   result: (symbol * pos) option,
		   body: exp,
		   pos: pos}
     
end
        
