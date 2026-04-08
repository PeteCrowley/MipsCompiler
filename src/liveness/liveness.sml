structure Liveness:
sig
  structure IGraph: FUNC_GRAPH
  datatype igraph =
    IGRAPH of
      { graph: IGraph.graph
      , tnode: Temp.temp -> IGraph.node
      , gtemp: IGraph.node -> Temp.temp
      , moves: (IGraph.node * IGraph.node) list
      }
  (* Input: flow graph, Output: interference graph and table of all
   * temporaries live-out at that node *)
  val interferenceGraph: Flow.flowgraph
                         -> igraph * (Graph.node -> Temp.temp list)
  val show: TextIO.outstream * igraph -> unit
end =
struct structure IGraph = MakeGraph() end
