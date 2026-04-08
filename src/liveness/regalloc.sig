signature REG_ALLOC =
sig
    structure Frame: FRAME
    type allocation = Frame.register Temp.Table.table
    val alloc: Assem.instr list * Frame.frame ->
                            Assem. instr list * allocation *
                            Liveness.output_igraph (* TODO: Testing, remove when done *)
end
