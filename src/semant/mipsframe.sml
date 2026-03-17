structure MipsFrame: FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, num_in_frame: int}

  fun name (frame: frame) = #name frame

  fun access_escapes (access: access) =
    case access of
      InFrame _ => false
    | InReg _ => true

  fun formals (frame: frame) = (#formals frame)

  fun newFrame {name: Temp.label, formals: bool list} =
    {name = name, formals = [], num_in_frame = 0}

  fun allocLocal (frame: frame) =
    let
      fun alloc_escapes (escapes: bool) = InFrame (0)
    in
      alloc_escapes
    end

end
