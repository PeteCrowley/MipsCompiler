structure TestManifest :> TEST_MANIFEST =
struct
  type test_entry =
    { test_name: string
    , test_dirs: string list
    , test_fn: string * string -> unit
    }

  fun echo (input, output) =
    let
      val is = TextIO.openIn input
      val os = TextIO.openOut output
      val () = TextIO.output (os, TextIO.input is)
      val () = TextIO.closeIn is
      val () = TextIO.closeOut os
    in
      ()
    end


  fun lex (input, output) =
    let
      fun do_it () =
        Lex.lex input
        handle Fail msg => print ("Program raised Fail: " ^ msg)
    in
      IOUtil.withOutputFile (output, do_it) ()
    end

  fun parse (input, output) =
    let
      val absyn = Parse.parse input
      val outputFile = TextIO.openOut output
    in
      PrintAbsyn.print (outputFile, absyn)
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  val allTests =
    [ { test_name = "echo"
      , test_dirs = ["appel-programs", "lexer-programs"]
      , test_fn = echo
      }
    , { test_name = "lex"
      , test_dirs = ["appel-programs", "lexer-programs"]
      , test_fn = lex
      }
    , { test_name = "parse"
      , test_dirs = ["appel-programs", "parser-programs"]
      , test_fn = parse
      }
    ]
end
