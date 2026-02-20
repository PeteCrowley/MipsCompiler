structure Types =
struct

  type unique = unit ref

  datatype ty =
  (*avoids using a second "name" type for recursive records*)
    RECORD of (unit -> ((Symbol.symbol * ty) list)) * unique
  | NIL (* subtype of all non-primitives *)
  | INT
  | STRING
  | ARRAY of ty * unique
  | ARROW of (ty list) * ty
  | UNIT (* top type *)
  | BOTTOM (* type of impossibility *)
end
