structure Liveness:
sig
  structure IGraph: FUNC_GRAPH
  (* TODO: clean up temp ord_key stuff ig *)
  structure TempMap: ORD_MAP where type Key.ord_key = Temp.Key.ord_key

  datatype output_igraph =
    IGRAPH of
      { graph: IGraph.graph
      , tnode: Temp.temp -> IGraph.node
      , gtemp: IGraph.node -> Temp.temp
      , moves: (IGraph.node * IGraph.node) list
      }

  type igraph =
    { graph: IGraph.graph
    , temp_to_node: IGraph.node TempMap.map
    , node_to_temp: Temp.temp IGraph.NodeMap.map
    , moves: (IGraph.node * IGraph.node) list
    }
  (* Input: flow graph, Output: interference graph and table of all
   * temporaries live-out at that node *)
  val interferenceGraph: Flow.flowgraph
                         -> output_igraph * (Flow.Graph.node -> Temp.temp list)

  val show: TextIO.outstream * output_igraph -> unit

  val to_output_igraph: igraph -> output_igraph
end =
struct
  structure IGraph = MakeGraph()

  structure TempMap: ORD_MAP = RedBlackMapFn(Temp.Key)
  (* structure TempMap: ORD_MAP = Temp.Map *)

  datatype output_igraph =
    IGRAPH of
      { graph: IGraph.graph
      , tnode: Temp.temp -> IGraph.node
      , gtemp: IGraph.node -> Temp.temp
      , moves: (IGraph.node * IGraph.node) list
      }

  type igraph =
    { graph: IGraph.graph
    , temp_to_node: IGraph.node TempMap.map
    , node_to_temp: Temp.temp IGraph.NodeMap.map
    , moves: (IGraph.node * IGraph.node) list
    }


  fun to_output_igraph (igraph: igraph) =
    let
      val {graph, temp_to_node, node_to_temp, moves} = igraph

      fun map_tnode ttn =
        (fn temp =>
           (case TempMap.find (ttn, temp) of
              SOME node => node
            | NONE => IGraph.find_unused_node graph))

      fun map_gtemp ntt =
        (fn node =>
           (case IGraph.NodeMap.find (ntt, node) of
              SOME temp => temp
            | NONE => ~1))
    in
      IGRAPH
        { graph = graph
        , tnode = map_tnode temp_to_node
        , gtemp = map_gtemp node_to_temp
        , moves = moves
        }
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

      fun node_to_str (node: IGraph.node) =
        "n" ^ IGraph.nodename (graph, node) ^ " : "
        ^ Temp.makestring (gtemp node)

      fun print_node_mappings () =
        let
          val node_strs = map node_to_str
            (IGraph.NodeSet.toList (IGraph.nodes graph))
          val all_nodes = String.concatWith "\n" node_strs
        in
          ( TextIO.output (outstream, all_nodes)
          ; TextIO.output (outstream, "\n")
          )
        end
    in
      (* IGraph.NodeSet.app (fn n => print_node (graph, n)) (IGraph.nodes graph) *)
      print_node_mappings ()
    end

  fun interferenceGraph flow_graph =
    let
      val interference_graph = IGraph.empty
      val (live_in, live_out) = Flow.livenessAnalysis flow_graph
      (* val temp_to_node: IGraph.node TempMap.map = TempMap.empty *)
      (* val node_to_temp: Temp.temp IGraph.NodeMap.map = IGraph.NodeMap.empty *)


      (* FIXME: either def a TempSet using Temp.Key, or refactor TempMap *)
      fun flow_lives node =
        Temp.Set.toList (Flow.Graph.NodeMap.lookup (live_out, node))

      (* TODO: write this monstrosity, use it in foldl_dfs *)
      (* visits one flowgraph node, creates corresponding edges *)
      fun dfs_traversal
        ( ( fgraph: Flow.flowgraph
          , live_out_dfs: Temp.Set.set Flow.Graph.NodeMap.map
          , igraph: igraph
          )
        , node: Flow.Graph.node
        ) : (Flow.flowgraph * Temp.Set.set Flow.Graph.NodeMap.map * igraph) =
        let
          (* val dbg_print_node = print *)
          (*   (Flow.Graph.nodename (#control fgraph, node) ^ "\n") *)
          val
            { control
            , def (* : Temp.temp list Flow.Graph.NodeMap.map *)
            , use (* : Temp.temp list Flow.Graph.NodeMap.map *)
            , ismove
            } = fgraph
          val node_is_move = Flow.Graph.NodeMap.lookup (ismove, node)

          (* Lookup temp in temp-to-node map, if not found, insert. *)
          (* igraph, temp -> igraph', node *)
          fun find_or_make_inode (igraph_fm: igraph, temp: Temp.temp) :
            (igraph * IGraph.node) =
            let
              (* val dbg_print_fm = print ("\t| " ^ Temp.makestring temp) *)
              val
                { graph: IGraph.graph
                , temp_to_node: IGraph.node TempMap.map
                , node_to_temp: Temp.temp IGraph.NodeMap.map
                , moves: (IGraph.node * IGraph.node) list
                } = igraph_fm
            in
              case TempMap.find (temp_to_node, temp) of
                SOME node =>
                  let
                  (* val dbg_print_found = print (" found.\n") *)
                  in (igraph_fm, node)
                  end
              | NONE =>
                  let
                    val new_node = IGraph.find_unused_node graph
                    val graph' = IGraph.addNode (graph, new_node)
                    val ttn' = TempMap.insert (temp_to_node, temp, new_node)
                    val ntt' =
                      IGraph.NodeMap.insert (node_to_temp, new_node, temp)
                  (* val dbg_print_make_new = print (" made.\n") *)
                  in
                    ( { graph = graph'
                      , temp_to_node = ttn'
                      , node_to_temp = ntt'
                      , moves = moves
                      }
                    , new_node
                    )
                  end
            end

          (* convert list of temps to list of nodes, while adding to igraph *)
          fun convert_temps (igraph_convert: igraph, temps: Temp.temp list) :
            (igraph * IGraph.node list) =
            let
              (* val dbg_print_temps = print *)
              (*   ("converting temps: " *)
              (*    ^ String.concatWith ", " (map Temp.makestring temps) ^ "\n") *)

              fun conv_one (temp, (igraph_conv, nodes)) =
                let
                  val (igraph', node') = find_or_make_inode (igraph_conv, temp)
                in
                  (igraph', node' :: nodes)
                end
            in
              foldl conv_one (igraph_convert, []) temps
            end

          val flow_defs: Temp.temp list = Flow.Graph.NodeMap.lookup (def, node)
          (* val inode_defs, igraph' *)

          fun add_edges (flow_defs_add, flow_lives, igraph_add: igraph) =
            let
              (* IGraph.graph * Igraph.node * Temp.Set.set * (IGraph.node *
               * IGraph.node) list -> IGraph.graph, Moves *)
              fun add_edge
                (dst: IGraph.node, src: IGraph.node, graph: IGraph.graph, moves) =
                if node_is_move then
                  ( IGraph.addEdge (graph, {from = src, to = dst})
                  , (src, dst) :: moves
                  )
                else
                  (IGraph.addEdge (graph, {from = src, to = dst}), moves)
              val (igraph', def_nodes) =
                convert_temps (igraph_add, flow_defs_add)
              val (igraph'', live_nodes) = convert_temps (igraph', flow_lives)
              (* val dbg_print_igraph'' = show *)
              (*   (TextIO.stdOut, (to_output_igraph igraph'')) *)
              val
                { graph: IGraph.graph
                , temp_to_node: IGraph.node TempMap.map
                , node_to_temp: Temp.temp IGraph.NodeMap.map
                , moves: (IGraph.node * IGraph.node) list
                } = igraph''

              (* run add_edge on all nodes in a list for a single def node *)
              fun single_def_edges
                (def_node, (live_nodes_single, graph_single, moves_single)) =
                let
                  val (graph', moves') =
                    foldl
                      (fn (live_node, (graph_lambda, moves_lambda)) =>
                         add_edge
                           (def_node, live_node, graph_lambda, moves_lambda))
                      (graph_single, moves_single) live_nodes_single
                in
                  (live_nodes_single, graph', moves')
                end

              (* the old returned value *)
              val (_ (*live nodes*), igraph_graph', moves') =
                foldl single_def_edges (live_nodes, graph, moves) def_nodes
            in
              { graph = igraph_graph'
              , temp_to_node = temp_to_node
              , node_to_temp = node_to_temp
              , moves = moves'
              }
            end

          (* val (_, igraph_graph', moves') = *)
          (*   add_edges (flow_defs, flow_lives node, igraph) *)

          (* so here's the problem. the maps are not getting passed through
           * correctly?
           * *)
          val igraph_out = add_edges (flow_defs, flow_lives node, igraph)
        (* { graph = igraph_graph' *)
        (* , temp_to_node = #temp_to_node igraph *)
        (* , node_to_temp = #node_to_temp igraph *)
        (* , moves = moves' *)
        (* } *)

        (* val dbg_print_igraph_out = *)
        (*   ( print ("igraph_out:\n") *)
        (*   ; show (TextIO.stdOut, (to_output_igraph igraph_out)) *)
        (*   ) *)

        (* val dbg_final_print = print ("\n") *)

        in
          (fgraph, live_out_dfs, igraph_out)
        end

      val base_igraph: igraph =
        { graph = IGraph.empty
        , temp_to_node = TempMap.empty
        , node_to_temp = IGraph.NodeMap.empty
        , moves = []
        }
      (* ( IGRAPH *)
      (*     { graph = interference_graph *)
      (*     , tnode = (fn _ => IGraph.find_unused_node interference_graph) *)
      (*     , gtemp = (fn _ => Temp.newtemp ()) *)
      (*     , moves = [] *)
      (*     } *)
      (* , (fn (node: Graph.node) => []) *)
      (* ) *)


      val (_ (*fgraph'*), _ (*live_out'*), igraph') =
        Flow.Graph.foldl_dfs dfs_traversal
          (Flow.Graph.NodeSet.toList (Flow.Graph.sources (#control flow_graph)))
          (flow_graph, live_out, base_igraph) (#control flow_graph)

    in
      (to_output_igraph igraph', flow_lives)
    end


end
