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

      (* The SML Int.toString uses ~ as the negative sign *)
      fun betterIntToString x =
        if x < 0 then "-" ^ Int.toString (~x) else Int.toString x

      fun munchStm (Tree.SEQ (a, b)) =
            (munchStm a; munchStm b)
        (* Store: MEM[i] = s0 *)
        | munchStm (Tree.MOVE (Tree.MEM (Tree.CONST i), e1)) =
            emit (Assem.OPER
              { assem = "    sw `s0, " ^ betterIntToString i ^ "($0)\n"
              , src = [munchExp e1]
              , dst = []
              , jump = NONE
              })
        (* Store: MEM[s1 + i] = s0 *)
        | munchStm
            (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, e1, Tree.CONST i)), e2)) =
            emit (Assem.OPER
              { assem = "    sw `s1, " ^ betterIntToString i ^ "(`s0)\n"
              , src = [munchExp e1, munchExp e2]
              , dst = []
              , jump = NONE
              })
        (* Store: MEM[i + s1] = s0 *)
        | munchStm
            (Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST i, e1)), e2)) =
            emit (Assem.OPER
              { assem = "    sw `s1, " ^ betterIntToString i ^ "(`s0)\n"
              , src = [munchExp e1, munchExp e2]
              , dst = []
              , jump = NONE
              })
        (* Store: MEM[s1] = s0 *)
        | munchStm (Tree.MOVE (Tree.MEM e1, e2)) =
            emit (Assem.OPER
              { assem = "    sw `s1, 0(`s0)\n"
              , src = [munchExp e1, munchExp e2]
              , dst = []
              , jump = NONE
              })
        (* Load: s1 = MEM[s0 + i] *)
        | munchStm
            (Tree.MOVE
               ( Tree.TEMP t
               , Tree.MEM (Tree.BINOP (Tree.PLUS, e1, Tree.CONST i))
               )) =
            emit (Assem.OPER
              { assem = "    lw `d0, " ^ betterIntToString i ^ "(`s0)\n"
              , src = [munchExp e1]
              , dst = [t]
              , jump = NONE
              })
        (* Load: s1 = MEM[i + s0] *)
        | munchStm
            (Tree.MOVE
               ( Tree.TEMP t
               , Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST i, e1))
               )) =
            emit (Assem.OPER
              { assem = "    lw `d0, " ^ betterIntToString i ^ "(`s0)\n"
              , src = [munchExp e1]
              , dst = [t]
              , jump = NONE
              })
        (* Load: s1 = MEM[s0] *)
        | munchStm (Tree.MOVE (Tree.TEMP t, Tree.MEM e1)) =
            emit (Assem.OPER
              { assem = "    lw `d0, 0(`s0)\n"
              , src = [munchExp e1]
              , dst = [t]
              , jump = NONE
              })
        (* Generic move *)
        | munchStm (Tree.MOVE (Tree.TEMP t, e1)) =
            emit
              (Assem.MOVE
                 {assem = "    move `d0, `s0\n", dst = t, src = munchExp e1})
        (* NOTE: any other move statement is disallowed *)
        | munchStm (Tree.MOVE _) =
            raise Fail "Disallowed left side of MOVE node in IR"

        | munchStm (Tree.LABEL l) =
            emit (Assem.LABEL {assem = Symbol.name l ^ ":\n", lab = l})

        | munchStm (Tree.JUMP (Tree.NAME lab, _)) =
            emit (Assem.OPER
              { assem = "    j " ^ Symbol.name lab ^ "\n"
              , src = []
              , dst = []
              , jump = SOME [lab]
              })

        (* This functions as the MIPS return statement *)
        | munchStm (Tree.JUMP (Tree.TEMP reg, _)) =
            emit (Assem.OPER
              { assem = "    jr `s0\n"
              (* Per Appel (see notes on `procEntryExit2`, we should mark all
               * callee-saved and special registers as used here so that they're
               * live throughout the body of the function.
               * Additionally, marking callee-saved registers as used here means
               * only the registers we explicitly save in the prologue will be
               * available in the epilogue.
               *)
              , src =
                  munchExp
                    (Tree.TEMP reg) (* Almost certainly $ra, but that's
                                       Appel's problem not mine *)
                  :: MipsFrame.specialregs @ MipsFrame.calleesaves
              , dst = []
              , jump = SOME [] (* Sink in the flow graph *)
              })
        | munchStm (Tree.JUMP _) =
            raise Fail "Unsupported jump statement in IR"
        (* For CJUMPS we can assume the CJUMP is followed immediately by it's false label *)
        | munchStm (Tree.CJUMP (relop, exp1, exp2, tlab, flab)) =
            let
              fun relopToBranch Tree.EQ = "beq"
                | relopToBranch Tree.NE = "bne"
                | relopToBranch Tree.LT = "blt"
                | relopToBranch Tree.LE = "ble"
                | relopToBranch Tree.GT = "bgt"
                | relopToBranch Tree.GE = "bge"
                | relopToBranch Tree.ULT = "bltu"
                | relopToBranch Tree.ULE = "bleu"
                | relopToBranch Tree.UGT = "bgtu"
                | relopToBranch Tree.UGE = "bgeu"
            in
              emit (Assem.OPER
                { assem =
                    "    " ^ relopToBranch relop ^ " `s0, `s1, "
                    ^ Symbol.name tlab ^ "\n"
                , src = [munchExp exp1, munchExp exp2]
                , dst = []
                , jump = SOME [tlab, flab]
                })
            end

        | munchStm (Tree.EXP e) =
            (munchExp e; ())

      and munchExp (Tree.TEMP t) = t
        | munchExp (Tree.CONST i) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    li `d0, " ^ betterIntToString i ^ "\n"
                , src = []
                , dst = [r]
                , jump = NONE
                }))
        | munchExp (Tree.NAME l) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    la `d0, " ^ Symbol.name l ^ "\n"
                , src = []
                , dst = [r]
                , jump = NONE
                }))
        | munchExp (Tree.ESEQ (s, e)) =
            (munchStm s; munchExp e)

        (* Call a static function *)
        | munchExp (Tree.CALL (Tree.NAME name, args)) =
            ( emit (Assem.OPER
                { assem = "    jal " ^ Symbol.name name ^ "\n"
                , src = munchArgs (0, args)
                , dst = calldefs
                , jump = NONE
                })
            ; MipsFrame.RV
            )
        (* Call a function from a register *)
        | munchExp (Tree.CALL (f, args)) =
            let
              val insns = emit (Assem.OPER
                { assem = "    jalr `s0\n"
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
                { assem = "    lw `d0, " ^ betterIntToString i ^ "(`s0)\n"
                , src = [munchExp e2]
                , dst = [r]
                , jump = NONE
                }))
        (* Load: MEM[i + s0] *)
        | munchExp (Tree.MEM (Tree.BINOP (Tree.PLUS, e2, Tree.CONST i))) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    lw `d0, " ^ betterIntToString i ^ "(`s0)\n"
                , src = [munchExp e2]
                , dst = [r]
                , jump = NONE
                }))
        (* Load: MEM[s0] *)
        | munchExp (Tree.MEM e2) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    lw `d0, 0(`s0)\n"
                , src = [munchExp e2]
                , dst = [r]
                , jump = NONE
                }))

        (* s0 + i *)
        | munchExp (Tree.BINOP (Tree.PLUS, e1, Tree.CONST i)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    addi `d0, `s0, " ^ betterIntToString i ^ "\n"
                , src = [munchExp e1]
                , dst = [r]
                , jump = NONE
                }))
        (* i + s0 *)
        | munchExp (Tree.BINOP (Tree.PLUS, Tree.CONST i, e1)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    addi `d0, `s0, " ^ betterIntToString i ^ "\n"
                , src = [munchExp e1]
                , dst = [r]
                , jump = NONE
                }))
        (* s0 - i *)
        | munchExp (Tree.BINOP (Tree.MINUS, e1, Tree.CONST i)) =
            result (fn r =>
              emit (Assem.OPER
                { assem = "    addi `d0, `s0, " ^ betterIntToString (~i) ^ "\n"
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
                  { assem = "    " ^ opcode ^ " `d0, `s0, `s1\n"
                  , src = [munchExp e1, munchExp e2]
                  , dst = [r]
                  , jump = NONE
                  }))
            end

      and munchArgs (idx, args) =
        let
          fun munchArgsRec (idx, [], acc) = acc
            | munchArgsRec (idx, arg :: args, acc) =
                let
                  val srctemp = munchExp arg
                  fun moveToArgReg (idx) =
                    let
                      val argtemp = List.nth (MipsFrame.argregs, idx)
                      val _ = emit
                        (Assem.MOVE
                           { assem = "    move `d0, `s0\n"
                           , src = srctemp
                           , dst = argtemp
                           })
                      (* Argument register marked as used in the CALL isntruction *)
                      val acc = argtemp :: acc
                    in
                      munchArgsRec (idx + 1, args, acc)
                    end
                  fun moveToStack (idx) =
                    let
                      val argNumInStack = idx - MipsFrame.argRegisterCount
                      val offset = argNumInStack * MipsFrame.wordsize
                      val sp = MipsFrame.SP
                      val _ = emit (Assem.OPER
                        { assem =
                            "    sw `s0, " ^ betterIntToString offset
                            ^ "(`s1)\n"
                        , src = [srctemp, sp]
                        , dst = []
                        , jump = NONE
                        })
                    in
                      munchArgsRec (idx + 1, args, acc)
                    end
                in
                  if idx < MipsFrame.argRegisterCount then moveToArgReg idx
                  else moveToStack idx
                end
        in
          munchArgsRec (idx, args, [])
        end


      (* Actually munch the provided statement *)
      val _ = munchStm stm
    in
      List.rev (!ilist)
    end
end
