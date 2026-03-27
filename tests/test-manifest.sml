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
      PrintAbsyn.print (outputFile, absyn);
      TextIO.closeOut outputFile
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  fun typecheck (input, output) =
    let
      val absyn = Parse.parse input
      (* val outputFile = TextIO.openOut output *)
      fun do_it () = (Semant.transProg absyn; ())
    in
      IOUtil.withOutputFile (output, do_it) ()
    (* PrintExpty.print (outputFile, expty); *)
    (* TextIO.closeOut outputFile *)
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  fun escape (input, output) =
    let
      val absyn = Parse.parse input
      (* val outputFile = TextIO.openOut output *)
      fun do_it () =
        (FindEscape.printEscapes := true; FindEscape.findEscape absyn)
    in
      IOUtil.withOutputFile (output, do_it) ()
    (* PrintExpty.print (outputFile, expty); *)
    (* TextIO.closeOut outputFile *)
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  fun ir (input, output) =
    let
      val () = Translate.resetFrags()
      val () = Temp.reset()
      val absyn = Parse.parse input
      fun do_it () = Semant.printIrTree absyn
    in
      IOUtil.withOutputFile (output, do_it) ()
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  structure Frame = MipsFrame

  fun instruction_selection(input, output) = 
    let
      fun emitproc out (Frame.PROC {body, frame}) =
        let
            val _ = print ("emit " ^ (Symbol.name (Frame.name frame)) ^ "\n")
            (*         val _ = Printtree.printtree(out,body); *)
            val stms = Canon.linearize body
            (*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
            val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
            val instrs = List.concat (map (MipsGen.codegen frame) stms')
            val format0 = Assem.format (Temp.makestring)
          in
            app (fn i => TextIO.output (out, format0 i)) instrs
          end
      | emitproc out (Frame.STRING (lab, s)) =
          TextIO.output (out, Frame.string (lab, s))

      val () = Translate.resetFrags()
      val () = Temp.reset()
      val absyn = Parse.parse input
      val frags = Semant.transProg absyn

      fun do_it () = app (emitproc (TextIO.stdOut)) frags
    in
      IOUtil.withOutputFile (output, do_it) ()
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
    , { test_name = "typecheck"
      , test_dirs = ["typecheck-programs", "appel-programs"]
      , test_fn = typecheck
      }
    , {test_name = "escape", test_dirs = ["escape-programs"], test_fn = escape}
    , {test_name = "ir", test_dirs = ["ir-programs", "appel-programs"], test_fn = ir}
    , {test_name = "selection", test_dirs = ["selection-programs", "appel-programs"], test_fn = instruction_selection}
    ]
end
