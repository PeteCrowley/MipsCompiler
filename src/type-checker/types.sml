structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (() -> ((Symbol.symbol * ty) list)) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | ARROW of (ty list) * ty
	  | UNIT

end

