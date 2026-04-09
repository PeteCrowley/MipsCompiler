signature FUNC_GRAPH =
sig
  eqtype node
  type graph

  structure NodeSet: ORD_SET where type Key.ord_key = node
  structure NodeMap: ORD_MAP where type Key.ord_key = node

  val nodes: graph -> NodeSet.set
  val succ: graph * node -> NodeSet.set
  val pred: graph * node -> NodeSet.set
  val adj: graph * node -> NodeSet.set (* succ+pred *)

  val reverse: graph -> graph
  val sources: graph -> NodeSet.set
  val sinks: graph -> NodeSet.set

  val empty: graph
  val find_unused_node: graph -> node
  val addNode: graph * node -> graph
  val addEdge: graph * {from: node, to: node} -> graph
  val rmEdge: graph * {from: node, to: node} -> graph
  val rmNode: graph * node -> graph

  val nodename: graph * node -> string

  val fold_dfs: (('a * node) -> 'a) -> node -> 'a -> graph -> 'a
  val foldl_dfs: (('a * node) -> 'a) -> node list -> 'a -> graph -> 'a

  val dbg_dump: graph -> string
  val dbg_nodename: node -> string
end
