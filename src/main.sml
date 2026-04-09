structure Main =
struct

  structure Tr = Translate
  structure Frame = MipsFrame
  structure R = RegAlloc

  fun emitproc out (Frame.PROC {body, frame}) =
        let
          (*         val _ = Printtree.printtree(out,body); *)
          val stms = Canon.linearize body
          (*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
          val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
          val instrs = List.concat (map (MipsGen.codegen frame) stms')
          val (newInstrs, regMap, igraph) = RegAlloc.alloc (instrs, frame)

          fun sayTemp i =
                case IntBinaryMap.find (regMap, i) of
                  SOME regName => regName
                | NONE => Temp.makestring i

          val format0 = Assem.format (sayTemp)
          
        in
          app (fn i => TextIO.output (out, format0 i)) instrs
        end
    | emitproc out (Frame.STRING (lab, s)) =
        TextIO.output (out, Frame.string (lab, s))


  fun printDotData out = TextIO.output (out, ".data\n")
  fun printDotText out = TextIO.output (out, ".text\n")

  fun copyRuntimeToBottomOfFile out =
    let
      val runtime = TextIO.openIn "src/assemruntime/runtime.s"
      fun copyLine () =
            case TextIO.inputLine runtime of
              SOME line => (TextIO.output (out, line); copyLine ())
            | NONE => ()
    in
      copyLine ();
      TextIO.closeIn runtime
    end

  fun withOpenFile fname f =
    let
      val out = TextIO.openOut fname
    in
      (f out before TextIO.closeOut out)
      handle e => (TextIO.closeOut out; raise e)
    end

  fun basename path =
    let
        fun loop ([], last) = last
          | loop (""::xs, last) = loop(xs, last)
          | loop (x::xs, _) = loop(xs, x)
    in
        loop (String.tokens (fn c => c = #"/") path, "")
    end
    

  fun compile filename =
    let
      val () = Translate.resetFrags ()
      val () = Temp.reset ()
      val absyn = Parse.parse filename
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
    in
      withOpenFile ("tests/output/assem/" ^ basename filename ^ ".s") (fn out => (
        printDotData out; 
        app (emitproc out) stringFrags; 
        printDotText out;
        app (emitproc out) funcFrags;
        copyRuntimeToBottomOfFile out))
    end

end
