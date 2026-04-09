structure MipsFrame:
sig
  include FRAME
  val SP: Temp.temp
  val RA: Temp.temp

  val specialregs: Temp.temp list
  val argregs: Temp.temp list
  val argRegisterCount: int
  val callersaves: Temp.temp list
  val calleesaves: Temp.temp list
  val getRegName: int -> string option
end =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame =
    { name: Temp.label
    , formals: access list
    , numLocalsInFrame: int ref
    (* If this function calls a function that requires 5+ args, MIPS requires
     * the stack be used. This gives the stack space required for arg passing *)
    , numWordsForStackArgPassing: int ref
    }
  type register = string

  datatype frag =
    PROC of {body: Tree.stm, frame: frame}
  | STRING of Temp.label * string

  val FP = 30
  val SP = 29
  val RA = 31
  val wordsize = 4
  val RV = 2
  val a0 = 4
  val fpOffset = ~wordsize
  val raOffset = ~2 * wordsize
  val numCalleeSaves = 8
  val saveOffset = ~3 * wordsize
  val slOffset = (~3 - numCalleeSaves) * wordsize

  local
    fun range (a, b) =
      List.tabulate (b - a + 1, fn i => a + i)
  in
    val specialregs =
      [0 (*r0*), 1 (*at*), 26 (*k0*), 27 (*k1*), 28 (*gp*), SP, FP, RA]
    val argregs = range (4, 7)
    val argRegisterCount = List.length argregs
    (* t registers *)
    val callersaves = range (8, 15) @ [24, 25]
    (* s registers *)
    val calleesaves = range (16, 23)
    val returnregs = range (2, 3)
    val precoloredTemps = specialregs @ argregs @ returnregs @ calleesaves @ callersaves
    val allregs = range (0, 31)
    val preferredRegOrder = SOME (callersaves @ calleesaves @ argregs @ returnregs @ specialregs)
  end

  fun getRegName(0) = SOME "$zero"
    | getRegName(1) = SOME "$at"
    | getRegName(2) = SOME "$v0"
    | getRegName(3) = SOME "$v1"
    | getRegName(4) = SOME "$a0"
    | getRegName(5) = SOME "$a1"
    | getRegName(6) = SOME "$a2"
    | getRegName(7) = SOME "$a3"
    | getRegName(8) = SOME "$t0"
    | getRegName(9) = SOME "$t1"
    | getRegName(10) =SOME "$t2"
    | getRegName(11) =SOME "$t3"
    | getRegName(12) =SOME "$t4"
    | getRegName(13) =SOME "$t5"
    | getRegName(14) =SOME "$t6"
    | getRegName(15) =SOME "$t7"
    | getRegName(16) =SOME "$s0"
    | getRegName(17) =SOME "$s1"
    | getRegName(18) = SOME "$s2"
    | getRegName(19) = SOME "$s3"
    | getRegName(20) = SOME "$s4"
    | getRegName(21) = SOME "$s5"
    | getRegName(22) = SOME "$s6"
    | getRegName(23) = SOME "$s7"
    | getRegName(24) = SOME "$t8"
    | getRegName(25) = SOME "$t9"
    | getRegName(26) = SOME "$k0"
    | getRegName(27) = SOME "$k1"
    | getRegName(28) = SOME "$gp"
    | getRegName(29) = SOME "$sp"
    | getRegName(30) = SOME "$fp"
    | getRegName(31) = SOME "$ra"
    | getRegName n = NONE

  val initialMappings = 
    let
      fun f (temp, acc) =
            case getRegName temp of
              SOME reg => Temp.Table.enter (acc, temp, reg)
            | NONE => acc (* never hit *)
    in
      List.foldl f Temp.Table.empty precoloredTemps
    end

  val registers = map (fn x => case getRegName x of SOME n => n | NONE => "") allregs

  fun name (frame: frame) = #name frame

  fun access_escapes (access: access) =
    case access of
      InFrame _ => false
    | InReg _ => true

  fun formals (frame: frame) = (#formals frame)

  fun allocLocal (frame: frame) =
    let
      fun allocLocalForFrame (false) =
            let val t = Temp.newtemp ()
            in InReg t
            end

        | allocLocalForFrame (true) =
            let
              val inFrameRef = #numLocalsInFrame frame
              val _ = inFrameRef := !inFrameRef + 1
            in
              InFrame (!inFrameRef * ~4)
            end
    in
      allocLocalForFrame
    end

  fun numWordsForStackArgPassing (frame: frame) =
    !(#numWordsForStackArgPassing frame)
  fun allocSpaceForStackArgs (frame: frame, numArgs) =
    let
      val numStackArgs = numArgs - argRegisterCount
      val frameRef = (#numWordsForStackArgPassing frame)
    in
      if numStackArgs > !frameRef then frameRef := numStackArgs else ()
    end

  fun exp (InFrame k) framePtrAddr =
        Tree.MEM (Tree.BINOP (Tree.PLUS, framePtrAddr, Tree.CONST k))
    | exp (InReg t) _ = Tree.TEMP t

  (* this is straight duplicate I'm too lazy not to -Pete *)
  fun seq [s] = s
    | seq (a :: l) =
        Tree.SEQ (a, seq l)
    | seq [] =
        Tree.EXP (Tree.CONST 0)


  fun prologue (label, stackSpace, formalAccesses) =
    let
      fun getArgLocation ind = if ind < argRegisterCount 
          then Tree.TEMP (List.nth (argregs, ind))
          else Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.TEMP FP, Tree.CONST ((ind - argRegisterCount) * wordsize)))


      val prologueCode =
            seq
              [ Tree.LABEL label
              , Tree.MOVE
                  ( Tree.MEM
                      (Tree.BINOP
                         (Tree.PLUS, Tree.TEMP SP, Tree.CONST fpOffset))
                  , Tree.TEMP FP
                  )
              , (* save old frame pointer in stack *)
                Tree.MOVE (Tree.TEMP FP, Tree.TEMP SP)
              , (* update frame pointer *)
                Tree.MOVE
                  ( Tree.TEMP SP
                  , Tree.BINOP (Tree.PLUS, Tree.TEMP SP, Tree.CONST (~stackSpace))
                  )
              , (* decrement stack pointer *)
                Tree.MOVE
                  ( Tree.MEM (Tree.BINOP
                      (Tree.PLUS, Tree.TEMP FP, Tree.CONST raOffset))
                  , Tree.TEMP RA
                  ) (* save RA *)
              ]
        fun saveCalleeSave (reg, (j, acc)) =
            (j-1, Tree.MOVE
              ( Tree.MEM (Tree.BINOP
                  (Tree.PLUS, Tree.TEMP FP, Tree.CONST (saveOffset - j * wordsize)))
              , Tree.TEMP reg
              ) :: acc)
        
        fun argMoveStatement (InReg r, (j, acc)) =
            (j-1, (Tree.MOVE (Tree.TEMP r, getArgLocation j) :: acc)) (* move args to temps *)
        | argMoveStatement (InFrame offs, (j, acc)) = 
            (j-1, (Tree.MOVE(Tree.MEM (Tree.BINOP(Tree.PLUS, Tree.TEMP FP, Tree.CONST offs)), getArgLocation j) :: acc)) (* move args to stack *)

        val (_, argMoves) =
          foldr argMoveStatement (List.length formalAccesses - 1, []) formalAccesses

        val (_, calleeSaveMoves) =
          foldr saveCalleeSave (numCalleeSaves - 1, []) calleesaves
    in
      seq [prologueCode, seq calleeSaveMoves, seq argMoves]
    end


  fun epilogue (body) =
    let
      val prologueCode = seq[Tree.MOVE (Tree.TEMP RA, Tree.MEM (Tree.BINOP
          (Tree.PLUS, Tree.TEMP FP, Tree.CONST raOffset)))
      , (* restore RA *)
        Tree.MOVE (Tree.TEMP SP, Tree.TEMP FP)
      , (* collapse stack frame *)
        Tree.MOVE (Tree.TEMP FP, Tree.MEM (Tree.BINOP
          (Tree.PLUS, Tree.TEMP FP, Tree.CONST fpOffset)))
      , (* restore FP *)
        Tree.JUMP (Tree.TEMP RA, [])
      ]
      fun restoreCalleeSave (reg, (j, acc)) =
            (j-1, Tree.MOVE
              ( Tree.TEMP reg
              , Tree.MEM (Tree.BINOP
                  (Tree.PLUS, Tree.TEMP FP, Tree.CONST (saveOffset - j * wordsize)))
              ) :: acc)
      val (_, calleeSaveRestores) =
        foldr restoreCalleeSave (numCalleeSaves - 1, []) calleesaves

    in
      seq [Tree.MOVE (Tree.TEMP RV, body), seq calleeSaveRestores, prologueCode]
    end
    
      

  fun framesize (frame: frame) =
    let
      val sizeForLocals = !(#numLocalsInFrame frame) * wordsize
      val sizeForArgs = !(#numWordsForStackArgPassing frame) * wordsize
    in
      sizeForLocals + sizeForArgs
    end

  (* not used at all right now can uncomment adding locals in new frame to make this work *)
  fun addPrologueEpliogue (frame: frame, bodyExp: Tree.exp) =
    (* code for moving args into general purpose registers *)
    let
      val stackSpace = framesize frame
      val label = #name frame
      val formalAccesses = #formals frame

    in
      Tree.SEQ (prologue (label, stackSpace, formalAccesses), epilogue bodyExp)
    end

  (* this may actually need to be more complicated idrk rn *)
  fun externalCall (funcName, args) =
    Tree.CALL (Tree.NAME (Temp.namedlabel funcName), args)

  (* To-do move this to .word and then .ascii *)
  fun string (lab, s) = Symbol.name lab ^ ": .word " ^ Int.toString (String.size s) ^ "\n" ^ ".ascii " ^ "\"" ^ s ^ "\"\n"

  fun newFrame {name: Temp.label, formals: bool list} =
    let
      fun oneFormalToAccess (true, (offset, formals)) =
            (offset + wordsize, (InFrame (~(offset+wordsize))) :: formals)
        | oneFormalToAccess (false, (offset, formals)) =
            (offset, (InReg (Temp.newtemp ())) :: formals)
      val raFpSpace = 2 * wordsize
      val raFpSaveSpace = raFpSpace + numCalleeSaves * wordsize
      val (numBytesForFormals, accesses) =
        foldl oneFormalToAccess (raFpSaveSpace, []) formals
      val f =
        { name = name
        , formals = List.rev accesses
        , numLocalsInFrame = ref (numBytesForFormals div wordsize)
        , numWordsForStackArgPassing = ref 0
        }
    in
      f
    end

end
