structure RegAlloc: REG_ALLOC =
struct
  structure Frame = MipsFrame
  type allocation = Frame.register Temp.Table.table
  fun alloc (instrs, frame) =
    let
      val (graph, nodeList) = MakeGraph.instrs2graph instrs
      val (interference, liveOut) = Liveness.interferenceGraph graph
      fun spillCost node = 1
      val (allocation, spilled) = Color.color
        { interference = interference
        , initial = Frame.initialMappings
        , spillCost = spillCost
        , registers = Frame.registers
        , prefRegOrder = Frame.preferredRegOrder
        }
    in
      ( instrs
      , allocation
      , interference
      ) (* instrs is fine for now because we won't do any edits for spilling *)
    end
end

