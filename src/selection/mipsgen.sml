structure MipsGen: CODEGEN =
struct
  structure Frame = MipsFrame
  (* All of the registers potentially overwritten by a function call *)
  val calldefs =
    MipsFrame.argregs @ MipsFrame.callersaves @ [MipsFrame.RV, MipsFrame.RA]
  fun codegen frame stm =
    let
      val ilist: Assem.instr list ref = ref []

      fun emit (insn) =
        ilist := insn :: !ilist

      fun result (gen) =
        let val t = Temp.newtemp ()
        in gen t; t
        end

      fun munchStm (Tree.SEQ (a, b)) =
            (munchStm a; munchStm b)
        (* Store: MEM[i] = s0 *)
        | munchStm (Tree.MOVE (Tree.MEM (Tree.CONST i), e1)) =
            emit (Assem.OPER
              { assem = "    sw 's0, " ^ Int.toString i ^ "($0)\n"
              , src = [munchExp e1]
              , dst = []
              , jump = NONE
              })
        (* Store: MEM[s1 + i] = s0 *)
        | munchStm
            (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, e1, Tree.CONST i)), e2)) =
            emit (Assem.OPER
              { assem = "    sw 's0, " ^ Int.toString i ^ "(s1)\n"
              , src = [munchExp e1, munchExp e2]
              , dst = []
              , jump = NONE
              })
        (* Store: MEM[i + s1] = s0 *)
        | munchStm
            (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST i, e1)), e2)) =
            emit (Assem.OPER
              { assem = "    sw 's0, " ^ Int.toString i ^ "(s1)\n"
              , src = [munchExp e1, munchExp e2]
              , dst = []
              , jump = NONE
              })
        (* Store: MEM[s1] = s0 *)
        | munchStm (Tree.MOVE (Tree.MEM e1, e2)) =
            emit (Assem.OPER
              { assem = "    sw 's0, 0(s1)\n"
              , src = [munchExp e1, munchExp e2]
              , dst = []
              , jump = NONE
              })
        (* Generic move *)
        | munchStm (Tree.MOVE (Tree.TEMP t, e1)) =
            emit
              (Assem.MOVE
                 {assem = "    move 'd0, 's0", dst = t, src = munchExp e1})
        (* NOTE: any other move statement is disallowed *)
        | munchStm (Tree.MOVE _) =
            raise Fail "Disallowed left side of MOVE node in IR"

        | munchStm (Tree.LABEL l) =
            emit (Assem.LABEL {assem = Symbol.name l ^ ":\n", lab = l})

        (* TODO: jumps *)
        | munchStm (Tree.JUMP _) = ()
        | munchStm (Tree.CJUMP _) = ()

        | munchStm (Tree.EXP e) =
            (munchExp e; ())

      and munchExp (Tree.TEMP t) = t
        (* TODO: this fails to assemble if i larger than 16-bit *)
        | munchExp (Tree.CONST i) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    li 'd0, " ^ Int.toString i ^ "\n"
                , src = []
                , dst = [r]
                , jump = NONE
                }))
        | munchExp (Tree.NAME l) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    la 'd0, " ^ Symbol.name l ^ "\n"
                , src = []
                , dst = [r]
                , jump = NONE
                }))
        | munchExp (Tree.ESEQ (s, e)) =
            (munchStm s; munchExp e)

        (* Call a static function *)
        | munchExp (Tree.CALL (Tree.NAME name, args)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "jal " ^ Symbol.name name ^ "\n"
                , src = munchArgs (0, args)
                , dst = calldefs
                , jump = NONE
                }))
        (* Call a function from a register *)
        | munchExp (Tree.CALL (f, args)) =
            let
              val insns = emit (Assem.OPER
                { assem = "jalr 's0\n"
                , src = munchExp f :: munchArgs (0, args)
                , dst = calldefs
                , jump = NONE
                })
            in
              MipsFrame.RV
            end

        (* Load: MEM[s0 + i] *)
        | munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST i, e2))) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    lw 'd0, " ^ Int.toString i ^ "('s0)\n"
                , src = [munchExp e2]
                , dst = [r]
                , jump = NONE
                }))
        (* Load: MEM[i + s0] *)
        | munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, e2, Tree.CONST i))) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    lw 'd0, " ^ Int.toString i ^ "('s0)\n"
                , src = [munchExp e2]
                , dst = [r]
                , jump = NONE
                }))
        (* Load: MEM[s0] *)
        | munchExp (Tree.MEM e2) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    lw 'd0, 0('s0)\n"
                , src = [munchExp e2]
                , dst = [r]
                , jump = NONE
                }))

        (* s0 + i *)
        | munchExp (Tree.BINOP (Tree.PLUS, e1, Tree.CONST i)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    addi 'd0, 's0, " ^ Int.toString i ^ "\n"
                , src = [munchExp e1]
                , dst = [r]
                , jump = NONE
                }))
        (* i + s0 *)
        | munchExp (Tree.BINOP (Tree.PLUS, Tree.CONST i, e1)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    addi 'd0, 's0, " ^ Int.toString i ^ "\n"
                , src = [munchExp e1]
                , dst = [r]
                , jump = NONE
                }))
        (* s0 - i *)
        | munchExp (Tree.BINOP (Tree.MINUS, e1, Tree.CONST i)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    addi 'd0, 's0, " ^ Int.toString (~i) ^ "\n"
                , src = [munchExp e1]
                , dst = [r]
                , jump = NONE
                }))
        (* TODO: optimize s0 * i and s0 / i based on value of i *)

        (* s0 <arbitrary binop> s1 *)
        | munchExp (Tree.BINOP (oper, e1, e2)) =
            let
              fun binopToOpcode Tree.PLUS = "add"
                | binopToOpcode Tree.MINUS = "sub"
                | binopToOpcode Tree.MUL = "mul"
                | binopToOpcode Tree.DIV = "div"
                | binopToOpcode Tree.AND = "and"
                | binopToOpcode Tree.OR = "or"
                | binopToOpcode Tree.LSHIFT = "sll"
                | binopToOpcode Tree.RSHIFT = "srl"
                | binopToOpcode Tree.ARSHIFT = "sra"
                | binopToOpcode Tree.XOR = "xor"
              val opcode = binopToOpcode oper
            in
              result (fn r =>
                emit (Assem.OPER
                  { assem = "    " ^ opcode ^ " 'd0, 's0, 's1\n"
                  , src = [munchExp e1, munchExp e2]
                  , dst = [r]
                  , jump = NONE
                  }))
            end

      and munchArgs (idx, []) = []
        | munchArgs (idx, arg :: args) =
            let
              val srctemp = munchExp arg
              fun moveToArgReg (idx) =
                let
                  val argtemp = List.nth (MipsFrame.argregs, idx)
                in
                  emit
                    (Assem.MOVE
                       { assem = "    move 'd0, 's0"
                       , src = srctemp
                       , dst = argtemp
                       })
                end
              fun moveToStack (idx) =
                let
                  val argNumInStack = idx - MipsFrame.argRegisterCount
                  val offset = argNumInStack * MipsFrame.wordsize
                  val spName = "$sp" (* TODO *)
                in
                  emit (Assem.OPER
                    { assem =
                        "    sw 's0, " ^ Int.toString offset ^ "(" ^ spName
                        ^ ")\n"
                    , src = [srctemp]
                    , dst = []
                    , jump = NONE
                    })
                end
              val munch =
                if idx > MipsFrame.argRegisterCount then moveToArgReg idx
                else moveToStack idx
            in
              srctemp :: munchArgs (idx + 1, args)
            end


      (* Actually munch the provided statement *)
      val _ = munchStm stm
    in
      List.rev (!ilist)
    end
end
