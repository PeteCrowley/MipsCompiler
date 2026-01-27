structure TestParse =
struct
  fun parse filename =
    let
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      val lexer = Mlex.makeLexer get
      fun do_it outs =
        let
          val t = lexer ()
          val new_outs = outs ^ "\n" ^ t
        in
          if substring (t, 0, 3) = "EOF" then new_outs else do_it (new_outs)
        end
      (* TODO: ensure all output makes it to the output file? *)
      val result = do_it ""
    in
      TextIO.closeIn file;
      result ^ "\n"
    end
end
