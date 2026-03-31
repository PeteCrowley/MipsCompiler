signature FUNC_GRAPH =
sig
  type node
  type nodeset
  type graph

  val nodes: graph -> nodeset
  val succ: graph * node -> nodeset
  val pred: graph * node -> nodeset
  val adj: graph * node -> nodeset (* succ+pred *)
  val eq: graph * node * node -> bool

  val empty: graph
  val find_unused_node: graph -> node
  val addNode: graph * node -> graph
  val addEdge: graph * {from: node, to: node} -> graph
  val rmEdge: graph * {from: node, to: node} -> graph
  val rmNode: graph * node -> graph

  val nodename: graph * node -> string
end
