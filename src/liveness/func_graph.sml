structure FuncGraph: FUNC_GRAPH =
struct
  structure IntOrd = struct type ord_key = int val compare = Int.compare end
  structure IntSet = RedBlackSetFn(IntOrd)
  structure IntMap = RedBlackMapFn(IntOrd)

  type node = int
  type nodeset = IntSet.set
  type graph =
    {nodes: nodeset, pred: nodeset IntMap.map, succ: nodeset IntMap.map}

  fun nodes (g: graph) = #nodes g

  (* Gets the smallest unused node in O(n) time *)
  fun find_unused_node (g: graph) =
    let
      val s = #nodes g
      fun loop i =
        if IntSet.member (s, i) then loop (i + 1) else i
    in
      loop 0
    end

  (* TODO *)
  fun nodename (_, n) = Int.toString n

  fun succ (g: graph, n) =
    case IntMap.find (#succ g, n) of
      SOME s => s
    | NONE => IntSet.empty
  fun pred (g: graph, n) =
    case IntMap.find (#pred g, n) of
      SOME s => s
    | NONE => IntSet.empty
  fun adj (g, n) =
    let
      val pred = pred (g, n)
      val succ = succ (g, n)
    in
      IntSet.union (pred, succ)
    end

  val empty = {nodes = IntSet.empty, pred = IntMap.empty, succ = IntMap.empty}

  fun addNode ({nodes, pred, succ}, n) =
    (* Ensure node exists in both maps *)
    let
      val nodes' = IntSet.add (nodes, n)
      val succ' =
        case IntMap.find (succ, n) of
          SOME _ => succ
        | NONE => IntMap.insert (succ, n, IntSet.empty)

      val pred' =
        case IntMap.find (pred, n) of
          SOME _ => pred
        | NONE => IntMap.insert (pred, n, IntSet.empty)
    in
      {nodes = nodes', succ = succ', pred = pred'}
    end

  fun addEdge (g, {from, to}) =
    let
      val {nodes, pred, succ} = addNode (addNode (g, to), from)
      (* update successors *)
      val succSet =
        case IntMap.find (succ, from) of
          SOME s => s
        | NONE => IntSet.empty
      val succ' = IntMap.insert (succ, from, IntSet.add (succSet, to))

      (* update predecessors *)
      val predSet =
        case IntMap.find (pred, to) of
          SOME s => s
        | NONE => IntSet.empty
      val pred' = IntMap.insert (pred, to, IntSet.add (predSet, from))
    in
      {nodes = nodes, succ = succ', pred = pred'}
    end

  fun rmEdge (g, {from, to}) =
    let
      val {nodes, succ, pred} = g
      val succ' =
        case IntMap.find (succ, from) of
          SOME s =>
            let val s' = IntSet.delete (s, to)
            in IntMap.insert (succ, from, s')
            end
        | NONE => succ
      val pred' =
        case IntMap.find (pred, to) of
          SOME s =>
            let val s' = IntSet.delete (s, from)
            in IntMap.insert (pred, to, s')
            end
        | NONE => pred
    in
      {nodes = nodes, succ = succ', pred = pred'}
    end

  fun rmNode (g, n) =
    let
      val {nodes, pred, succ} = g
      val succs =
        case IntMap.find (succ, n) of
          SOME s => IntSet.listItems s
        | NONE => []
      val preds =
        case IntMap.find (pred, n) of
          SOME s => IntSet.listItems s
        | NONE => []
      (* Remove n from successors of its predecessors *)
      fun removeFromSucc (p, m) =
        case IntMap.find (m, p) of
          SOME s =>
            let val s' = IntSet.delete (s, n)
            in IntMap.insert (m, p, s')
            end
        | NONE => m
      val succ' = List.foldl removeFromSucc succ preds
      (* Remove n from predecessors of its successors *)
      fun removeFromPred (s, m) =
        case IntMap.find (m, s) of
          SOME ps =>
            let val ps' = IntSet.delete (ps, n)
            in IntMap.insert (m, s, ps')
            end
        | NONE => m
      val pred' = List.foldl removeFromPred pred succs
      (* Finally remove n itself *)
      val succ'' = IntMap.remove (succ', n)
      val pred'' = IntMap.remove (pred', n)
      val nodes' = IntSet.delete (nodes, n)
    in
      {nodes = nodes', succ = succ'', pred = pred''}
    end
end
