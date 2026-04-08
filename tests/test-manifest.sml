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
      fun do_it () =
        (Semant.transProg absyn; ())
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
      val () = Translate.resetFrags ()
      val () = Temp.reset ()
      val absyn = Parse.parse input
      fun do_it () = Semant.printIrTree absyn
    in
      IOUtil.withOutputFile (output, do_it) ()
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  structure Frame = MipsFrame

  fun instruction_selection (input, output) =
    let
      fun sayTemp i = case Frame.getRegName i of
                        SOME name => name
                      | NONE => Temp.makestring i

      fun emitproc out (Frame.PROC {body, frame}) =
            let
              val stms = Canon.linearize body
              val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
              val instrs = List.concat (map (MipsGen.codegen frame) stms')
              val format0 = Assem.format (sayTemp)
            in
              app (fn i => TextIO.output (out, format0 i)) instrs
            end
        | emitproc out (Frame.STRING (lab, s)) =
            TextIO.output (out, Frame.string (lab, s))

      val () = Translate.resetFrags ()
      val () = Temp.reset ()
      val absyn = Parse.parse input
      val frags = Semant.transProg absyn
      val stringFrags =
        List.filter
          (fn f =>
             case f of
               Frame.STRING _ => true
             | _ => false) frags
      val funcFrags =
        List.filter
          (fn f =>
             case f of
               Frame.PROC _ => true
             | _ => false) frags

      fun do_it () =
        ( app (emitproc (TextIO.stdOut)) stringFrags
        ; app (emitproc (TextIO.stdOut)) funcFrags
        )
    in
      IOUtil.withOutputFile (output, do_it) ()
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  fun liveness (input, output) =
    let
      fun emitproc out (Frame.PROC {body, frame}) =
            let
              val stms = Canon.linearize body
              val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
              val instrs = List.concat (map (MipsGen.codegen frame) stms')

              (* Liveness analysis *)
              val (fgraph, instrNodes) = MakeGraph.instrs2graph instrs
              val (liveIn, liveOut) = Flow.livenessAnalysis fgraph

              (* Debug print the flow graph *)
              val _ = TextIO.output (out, Flow.Graph.dbg_dump (#control fgraph))

              val format0 = Assem.format (Temp.makestring)

              (* Package the flow node and liveness data with the instruction *)
              val instrsAndLiveness =
                let
                  val instrsAndNodes = ListPair.zip (instrs, instrNodes)
                  fun getLiveness (instr, node) =
                    let
                      val liveIn = Flow.Graph.NodeMap.lookup (liveIn, node)
                      val liveOut = Flow.Graph.NodeMap.lookup (liveOut, node)
                    in
                      {instr = instr, node = node, liveness = (liveIn, liveOut)}
                    end
                in
                  List.map getLiveness instrsAndNodes
                end

              fun printLine line =
                let
                  fun formatNode node =
                    let
                      val {control = _, def, use, ismove} = fgraph
                      val def = Flow.Graph.NodeMap.lookup (def, node)
                      val use = Flow.Graph.NodeMap.lookup (use, node)
                      val ismove = Flow.Graph.NodeMap.lookup (ismove, node)

                      val def = map Temp.makestring def
                      val use = map Temp.makestring use
                      val defsLine = "\t| defs: " ^ String.concatWith ", " def
                      val usesLine = "\t| uses: " ^ String.concatWith ", " use
                    in
                      defsLine ^ "\n" ^ usesLine ^ "\n" ^ "\t| ismove: "
                      ^ Bool.toString ismove ^ "\n"
                    end
                  fun formatLiveness (liveIn, liveOut) =
                    let
                      fun fmtSet set =
                        String.concatWith ", "
                          (map Temp.makestring (Temp.Set.toList set))
                    in
                      "\t| live-in: " ^ fmtSet liveIn ^ "\n\t| live-out: "
                      ^ fmtSet liveOut ^ "\n"
                    end
                  val {instr, node, liveness} = line
                in
                  format0 instr ^ formatNode node ^ formatLiveness liveness
                end
            in
              app (fn i => TextIO.output (out, printLine i)) instrsAndLiveness
            end
        | emitproc out (Frame.STRING (lab, s)) =
            TextIO.output (out, Frame.string (lab, s))

      val () = Translate.resetFrags ()
      val () = Temp.reset ()
      val absyn = Parse.parse input
      val frags = Semant.transProg absyn

      fun do_it () =
        app (emitproc TextIO.stdOut) frags
    in
      IOUtil.withOutputFile (output, do_it) ()
    end
    handle Fail msg => print ("Program raised Fail: " ^ msg)

  fun regalloc (input, output) =
    let
                      
      fun emitproc out (Frame.PROC {body, frame}) =
            let
              val stms = Canon.linearize body
              val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
              val instrs = List.concat (map (MipsGen.codegen frame) stms')
              val (newInstrs, regMap) = RegAlloc.alloc (instrs, frame)

              fun sayTemp i = case IntBinaryMap.find (regMap, i) of
                                SOME regName => regName
                              | NONE => Temp.makestring i

              fun printMap map =
                let
                  val items = IntBinaryMap.listItemsi map
                  fun formatItem (temp, reg) =
                    let
                      val tempStr = Temp.makestring temp
                    in
                      "\t" ^ tempStr ^ " -> " ^ reg ^ "\n"
                    end
                in
                  String.concat (List.map formatItem items)
                end


              val format0 = Assem.format (sayTemp)
            in
              (* app (fn i => TextIO.output (out, format0 i)) newInstrs *)
              TextIO.output (out, printMap regMap)
            end
        | emitproc out (Frame.STRING (lab, s)) =
            TextIO.output (out, Frame.string (lab, s))

      val () = Translate.resetFrags ()
      val () = Temp.reset ()
      val absyn = Parse.parse input
      val frags = Semant.transProg absyn
      val stringFrags =
        List.filter
          (fn f =>
             case f of
               Frame.STRING _ => true
             | _ => false) frags
      val funcFrags =
        List.filter
          (fn f =>
             case f of
               Frame.PROC _ => true
             | _ => false) frags

      fun do_it () =
        ( app (emitproc (TextIO.stdOut)) stringFrags
        ; app (emitproc (TextIO.stdOut)) funcFrags
        )
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
    , { test_name = "ir"
      , test_dirs = ["ir-programs", "appel-programs"]
      , test_fn = ir
      }
    , { test_name = "selection"
      , test_dirs = ["selection-programs"]
      , test_fn = instruction_selection
      }
    , { test_name = "liveness"
      , test_dirs = ["selection-programs"]
      , test_fn = liveness
      }
    , { test_name = "regalloc"
      , test_dirs = ["selection-programs"]
      , test_fn = regalloc
      }
    ]
end
