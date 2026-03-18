structure MipsFrame: FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame =
    {name: Temp.label, formals: access list, num_locals_in_frame: int ref}

  fun name (frame: frame) = #name frame

  fun access_escapes (access: access) =
    case access of
      InFrame _ => false
    | InReg _ => true

  fun formals (frame: frame) = (#formals frame)

  fun newFrame {name: Temp.label, formals: bool list} =
    let
      fun oneFormalToAccess (true, (offset, formals)) =
            (offset + 4, (InFrame offset) :: formals)
        | oneFormalToAccess (false, (offset, formals)) =
            (offset, (InReg (Temp.newtemp ())) :: formals)
      val (numBytesForFormals, accesses) =
        foldr oneFormalToAccess (0, []) formals
    in
      {name = name, formals = accesses, num_locals_in_frame = ref 0}
    end

  fun allocLocal (frame: frame) =
    let
      fun allocLocalForFrame (false) =
            InReg (Temp.newtemp ())
        | allocLocalForFrame (true) =
            let
              val inFrameRef = #num_locals_in_frame frame
              val _ = inFrameRef := !inFrameRef + 1
            in
              InFrame (!inFrameRef * ~4)
            end
    in
      allocLocalForFrame
    end

end
