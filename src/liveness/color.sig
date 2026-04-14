signature COLOR =
sig
  structure Frame: FRAME
  type allocation = Frame.register Temp.Table.table
  val color:
    { interference: Liveness.output_igraph
    , initial: allocation
    , spillCost: Graph.node -> int
    , registers: Frame.register list
    , prefRegOrder: int list option
    }
    -> allocation * Temp.temp list
end
