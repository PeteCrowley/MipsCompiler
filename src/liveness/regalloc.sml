structure RegAlloc : REG_ALLOC = 
struct
    structure Frame = MipsFrame
    type allocation = Frame.register Temp.Table.table
    fun alloc (instrs, frame) = (instrs, Temp.Table.empty)
end