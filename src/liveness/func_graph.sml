(* Unit functor instead of a structure to allow creation of different graph
 * types (flow, interference) *)
functor MakeGraph () :> FUNC_GRAPH =
struct
  structure NodeOrd = struct type ord_key = int val compare = Int.compare end
  structure NodeSet = RedBlackSetFn(NodeOrd)
  structure NodeMap = RedBlackMapFn(NodeOrd)

  type node = int
  type graph =
    { unused_node: node (* Must be larger than any node in use *)
    , nodes: NodeSet.set
    , pred: NodeSet.set NodeMap.map
    , succ: NodeSet.set NodeMap.map
    }

  fun nodes (g: graph) = #nodes g

  (* Gets some unused node in O(1) time *)
  fun find_unused_node (g: graph) = #unused_node g

  (* TODO *)
  fun nodename (_, n) = Int.toString n

  fun succ (g: graph, n) =
    case NodeMap.find (#succ g, n) of
      SOME s => s
    | NONE => NodeSet.empty
  fun pred (g: graph, n) =
    case NodeMap.find (#pred g, n) of
      SOME s => s
    | NONE => NodeSet.empty
  fun adj (g, n) =
    let
      val pred = pred (g, n)
      val succ = succ (g, n)
    in
      NodeSet.union (pred, succ)
    end

  fun reverse graph =
    let val {unused_node, nodes, pred, succ} = graph
    in {unused_node = unused_node, nodes = nodes, pred = succ, succ = pred}
    end

  fun sources graph =
    let
      fun isSource node =
        NodeSet.isEmpty (pred (graph, node))
      val {unused_node, nodes, pred, succ} = graph
    in
      NodeSet.filter isSource nodes
    end
  fun sinks graph =
    sources (reverse graph)

  val empty =
    { unused_node = 0
    , nodes = NodeSet.empty
    , pred = NodeMap.empty
    , succ = NodeMap.empty
    }

  fun addNode ({unused_node, nodes, pred, succ}, n) =
    (* Ensure node exists in both maps *)
    let
      val nodes' = NodeSet.add (nodes, n)
      val unused_node' = Int.max (unused_node, n + 1)
      val succ' =
        case NodeMap.find (succ, n) of
          SOME _ => succ
        | NONE => NodeMap.insert (succ, n, NodeSet.empty)

      val pred' =
        case NodeMap.find (pred, n) of
          SOME _ => pred
        | NONE => NodeMap.insert (pred, n, NodeSet.empty)
    in
      {unused_node = unused_node', nodes = nodes', succ = succ', pred = pred'}
    end

  fun addEdge (g, {from, to}) =
    let
      val {unused_node, nodes, pred, succ} = addNode (addNode (g, to), from)
      (* update successors *)
      val succSet =
        case NodeMap.find (succ, from) of
          SOME s => s
        | NONE => NodeSet.empty
      val succ' = NodeMap.insert (succ, from, NodeSet.add (succSet, to))

      (* update predecessors *)
      val predSet =
        case NodeMap.find (pred, to) of
          SOME s => s
        | NONE => NodeSet.empty
      val pred' = NodeMap.insert (pred, to, NodeSet.add (predSet, from))
    in
      {unused_node = unused_node, nodes = nodes, succ = succ', pred = pred'}
    end

  fun rmEdge (g, {from, to}) =
    let
      val {unused_node, nodes, succ, pred} = g
      val succ' =
        case NodeMap.find (succ, from) of
          SOME s =>
            let val s' = NodeSet.delete (s, to)
            in NodeMap.insert (succ, from, s')
            end
        | NONE => succ
      val pred' =
        case NodeMap.find (pred, to) of
          SOME s =>
            let val s' = NodeSet.delete (s, from)
            in NodeMap.insert (pred, to, s')
            end
        | NONE => pred
    in
      {unused_node = unused_node, nodes = nodes, succ = succ', pred = pred'}
    end

  fun rmNode (g, n) =
    let
      val {unused_node, nodes, pred, succ} = g
      val succs =
        case NodeMap.find (succ, n) of
          SOME s => NodeSet.listItems s
        | NONE => []
      val preds =
        case NodeMap.find (pred, n) of
          SOME s => NodeSet.listItems s
        | NONE => []
      (* Remove n from successors of its predecessors *)
      fun removeFromSucc (p, m) =
        case NodeMap.find (m, p) of
          SOME s =>
            let val s' = NodeSet.delete (s, n)
            in NodeMap.insert (m, p, s')
            end
        | NONE => m
      val succ' = List.foldl removeFromSucc succ preds
      (* Remove n from predecessors of its successors *)
      fun removeFromPred (s, m) =
        case NodeMap.find (m, s) of
          SOME ps =>
            let val ps' = NodeSet.delete (ps, n)
            in NodeMap.insert (m, s, ps')
            end
        | NONE => m
      val pred' = List.foldl removeFromPred pred succs
      (* Finally remove n itself *)
      val (succ'', _) = NodeMap.remove (succ', n)
      val (pred'', _) = NodeMap.remove (pred', n)
      val nodes' = NodeSet.delete (nodes, n)
    in
      {unused_node = unused_node, nodes = nodes', succ = succ'', pred = pred''}
    end

  fun fold_dfs f root init g =
    let
      fun dfs (node, (acc, visited)) =
        let
          val succ = succ (g, node)
          val visitedThisNode = NodeSet.member (visited, node)
          (* Recursively search all children of the graph *)
          fun searchChildren (a, v) =
            let
              (* Apply accumulator function for current node *)
              val acc' = f (a, node)
              (* Mark current node as visited *)
              val visited' = NodeSet.add (v, node)
            in
              NodeSet.foldl dfs (acc', visited') succ
            end
        in
          if visitedThisNode then
            (* Already visited; skip iteration *)
            (acc, visited)
          else
            (* Not visited; search all the children *)
            searchChildren (acc, visited)
        end
      val init_visited = NodeSet.empty
      val (acc, _) = dfs (root, (init, init_visited))
    in
      acc
    end
  fun foldl_dfs f roots init g =
    let
      fun fold (root, acc) =
        fold_dfs f root acc g
    in
      foldl fold init roots
    end

  fun dbg_dump (g: graph) =
    let
      fun fmtSet set =
        String.concatWith ", "
          (map (fn n => nodename (g, n)) (NodeSet.toList set))

      val stringNodes =
        let
          fun nodename_better n = nodename (g, n)
          val nodesList = NodeSet.toList (#nodes g)
        in
          String.concatWith ", " (map nodename_better nodesList)
        end
      val stringEdges =
        let
          val allEdges =
            let
              val edgeSets = NodeMap.listItemsi (#succ g)
              fun unfoldEdgeSet (n, set) =
                map (fn m => (n, m)) (NodeSet.toList set)
              val edgeSetListSegments = map unfoldEdgeSet edgeSets
            in
              List.concat edgeSetListSegments
            end
          fun formatEdge (m, n) =
            nodename (g, m) ^ "->" ^ nodename (g, n)
          val formattedEdges = map formatEdge allEdges
        in
          String.concatWith ", " formattedEdges
        end
      val graph_sources = fmtSet (sources g)
      val graph_sinks = fmtSet (sinks g)
    in
      "nodes: " ^ stringNodes ^ "\nedges: " ^ stringEdges ^ "\nsources: "
      ^ graph_sources ^ "\nsinks: " ^ graph_sinks ^ "\n"
    end
end
