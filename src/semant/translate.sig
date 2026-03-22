signature TRANSLATE =
sig
    type level
    type access
    type exp

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access
    val getDummyExp: unit -> exp
    val simpleVar : access * level -> exp
    val intExp : int -> exp
    val assignExp : exp * exp -> exp
    val printTree : exp -> unit
    val expList : exp list -> exp
    val functionDec : level * exp -> exp
    val functionCall : level * level * Temp.label * exp list -> exp
end

