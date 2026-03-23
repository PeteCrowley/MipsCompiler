signature TRANSLATE =
sig
    type level
    type access
    type exp
    structure Frame : FRAME

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val getResult : unit -> Frame.frag list
    val formals : level -> access list
    val allocLocal : level -> bool -> access
    val getDummyExp: unit -> exp
    val getZeroExp: unit -> exp
    val simpleVar : access * level -> exp
    val intExp : int -> exp
    val assignExp : exp * exp -> exp
    val printTree : exp -> unit
    val expList : exp list -> exp
    val functionDec : level * exp -> unit
    val functionCall : level * level * Temp.label * exp list -> exp
    val addExp : exp * exp -> exp
    val subExp : exp * exp -> exp
    val mulExp : exp * exp -> exp
    val divExp : exp * exp -> exp
    val ltExp : exp * exp -> exp
    val leExp : exp * exp -> exp
    val gtExp : exp * exp -> exp
    val geExp : exp * exp -> exp
    val eqExp : exp * exp -> exp
    val neqExp : exp * exp -> exp
    val strLtExp : exp * exp -> exp
    val strLeExp : exp * exp -> exp
    val strGtExp : exp * exp -> exp
    val strGeExp : exp * exp -> exp
    val strEqExp : exp * exp -> exp
    val strNeqExp : exp * exp -> exp
    val ifExp: exp * exp * exp -> exp
    val whileExp: exp * exp * Temp.label -> exp
    val forExp: exp * exp * exp * exp * Temp.label -> exp
    val breakExp: Temp.label -> exp
    val stringLit: string -> exp
    val arrayExp: exp * exp -> exp
    val arrayAcessExp: exp * exp -> exp
    val recordExp: exp list -> exp
    val recordAccessExp: exp * int -> exp

end

