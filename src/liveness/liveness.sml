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
struct
  structure IGraph = MakeGraph()

  structure TempMap: ORD_MAP = RedBlackMapFn(Temp.Key)
  (* structure TempMap: ORD_MAP = Temp.Map *)

  datatype igraph =
    IGRAPH of
      { graph: IGraph.graph
      , tnode: Temp.temp -> IGraph.node
      , gtemp: IGraph.node -> Temp.temp
      , moves: (IGraph.node * IGraph.node) list
      }

  fun interferenceGraph (flow_graph) =
    let
      val interference_graph = IGraph.empty
      val (live_in, live_out) = Flow.livenessAnalysis flow_graph
      val temp_to_node: Temp.temp IGraph.NodeMap.map = IGraph.NodeMap.empty
      val node_to_temp: IGraph.node TempMap.map = TempMap.empty

      (* TODO: write this monstrosity, use it in foldl_dfs *)
      fun dfs_traversal
        ( fgraph: Flow.flowgraph
        , live_out
        , node: Flow.Graph.node
        , IGRAPH {graph, tnode, gtemp, moves}
        ) =
        let
          val node_is_move = Flow.Graph.NodeMap.lookup (#ismove fgraph, node)
          val neighbors = Flow.Graph.succ (#control fgraph, node)
          val node_defs = Flow.Graph.NodeMap.lookup (#def fgraph, node)
        in
          ()
        end

      val base_igraph =
        ( IGRAPH
            { graph = interference_graph
            , tnode = (fn _ => IGraph.find_unused_node interference_graph)
            , gtemp = (fn _ => Temp.newtemp ())
            , moves = []
            }
        , (fn (node: Graph.node) => [])
        )
    in
      ( dfs_traversal
          ( flow_graph
          , Flow.livenessAnalysis flow_graph
          , Flow.Graph.find_unused_node (#control flow_graph)
          , #1 base_igraph
          )
      ; base_igraph
      )
    end

  (* TODO: replace name with gtemp once implemented *)
  fun show (outstream, IGRAPH {graph: IGraph.graph, tnode, gtemp, moves}) =
    let
      fun print_succ (graph: IGraph.graph, node: IGraph.node) =
        IGraph.NodeSet.app
          (fn n =>
             TextIO.output (outstream, (IGraph.nodename (graph, n) ^ ", ")))
          (IGraph.succ (graph, node))

      fun print_node (graph: IGraph.graph, node: IGraph.node) =
        ( TextIO.output (outstream, IGraph.nodename (graph, node) ^ "->")
        ; print_succ (graph, node)
        ; TextIO.output (outstream, "\n")
        )
    in
      IGraph.NodeSet.app (fn n => print_node (graph, n)) (IGraph.nodes graph)
    end

end
