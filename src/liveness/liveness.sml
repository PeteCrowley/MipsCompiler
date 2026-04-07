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
      val temp_to_node: IGraph.node TempMap.map = TempMap.empty
      val node_to_temp: Temp.temp IGraph.NodeMap.map = IGraph.NodeMap.empty

      fun map_tnode (ttn) =
        (fn temp =>
           (case TempMap.find (ttn, temp) of
              SOME node => node
            | NONE => IGraph.find_unused_node (interference_graph)))

      fun map_gtemp (ntt) =
        (fn node =>
           (case IGraph.NodeMap.find (ntt, node) of
              SOME temp => temp
            | NONE => Temp.newtemp ()))

      (* TODO: write this monstrosity, use it in foldl_dfs *)
      (* visits one flowgraph node, creates corresponding edges *)
      fun dfs_traversal
        ( fgraph: Flow.flowgraph
        , live_out: Temp.Set.set Flow.Graph.NodeMap.map
        , node: Flow.Graph.node
        , igraph: igraph
        ) =
        let
          val
            { control
            , def (* : Temp.temp list Flow.Graph.NodeMap.map *)
            , use (* : Temp.temp list Flow.Graph.NodeMap.map *)
            , ismove
            } = fgraph
          val node_is_move = Flow.Graph.NodeMap.lookup (ismove, node)

          (* Lookup temp in temp-to-node map, if not found, insert. *)
          (* temp -> igraph, node *)
          fun find_or_make_inode (temp: Temp.temp) : (igraph * IGraph.node) =
            let
              val
                IGRAPH
                  { graph: IGraph.graph
                  , tnode: Temp.temp -> IGraph.node
                  , gtemp: IGraph.node -> Temp.temp
                  , moves: (IGraph.node * IGraph.node) list
                  } = igraph
            in
              case TempMap.find (temp_to_node, temp) of
                SOME node => (igraph, node)
              | NONE =>
                  let
                    val new_node = IGraph.find_unused_node graph
                    val graph' = IGraph.addNode (graph, new_node)
                    val tnode' = TempMap.insert (temp_to_node, temp, new_node)
                    val gtemp' =
                      IGraph.NodeMap.insert (node_to_temp, new_node, temp)
                  in
                    ( IGRAPH
                        { graph = graph'
                        , tnode = map_tnode (tnode')
                        , gtemp = map_gtemp (gtemp')
                        , moves = moves
                        }
                    , new_node
                    )
                  end
            end

          val flow_defs: Temp.temp list = Flow.Graph.NodeMap.lookup (def, node)
          (* val inode_defs, igraph' *)
          val flow_lives = Flow.Graph.NodeMap.lookup (live_out, node)

          (* IGraph.graph * Igraph.node * Temp.Set.set * (IGraph.node *
           * IGraph.node) list -> IGraph.graph, Moves *)
          fun add_edge
            (graph: IGraph.graph, dst: IGraph.node, src: IGraph.node, moves) =
            if node_is_move then
              ( IGraph.addEdge (graph, {from = src, to = dst})
              , (src, dst) :: moves
              )
            else
              (IGraph.addEdge (graph, {from = src, to = dst}), moves)

          (* run add_edge on all nodes in a list *)
          fun add_edges (graph, def_node, live_nodes, moves) =
            foldl
              (fn (live_node, (graph, moves)) =>
                 add_edge (graph, def_node, live_node, moves)) (graph, moves)
              live_nodes

          val neighbors = Flow.Graph.succ (control, node)
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
      (* ( dfs_traversal *)
      (*     ( flow_graph *)
      (*     , Flow.livenessAnalysis flow_graph *)
      (*     , Flow.Graph.find_unused_node (#control flow_graph) *)
      (*     , #1 base_igraph *)
      (*     ) *)
      base_igraph
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
