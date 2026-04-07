structure Color : COLOR =
struct
    structure Frame = MipsFrame
    type allocation = Frame.register Temp.Table.table
    structure NodeSet = Liveness.IGraph.NodeSet
    structure NodeMap = Liveness.IGraph.NodeMap

    structure NodePairOrd = struct
      type ord_key = Liveness.IGraph.node * Liveness.IGraph.node
        fun compare ((a1, b1), (a2, b2)) =
        case NodeSet.Key.compare (a1, a2) of
          EQUAL => NodeSet.Key.compare (b1, b2)
              | other => other
    end
    structure NodePairSet = RedBlackSetFn(NodePairOrd)

    fun color {interference: Liveness.igraph, initial, spillCost, registers} = 
        let
          val K = List.length registers
          val Liveness.IGRAPH {graph, tnode, gtemp, moves} = interference
          fun degree (node, igraph) = NodeSet.numItems (Liveness.IGraph.adj (igraph, node))
          
          fun makeWorklists () =
            let
                fun makeListHelper (node, (accSimp, accSpill, accFreeze, accMoves, accMoveMap)) =
                    let
                      val movesInvolvedWith = (List.filter (fn (u, v) => u = node orelse v = node) moves)

                    in
                      if degree (node, graph) >= K then (accSimp, NodeSet.add (accSpill, node), accFreeze, accMoves, accMoveMap)
                      else if not (List.length movesInvolvedWith = 0) then (accSimp, accSpill, 
                                                                            NodeSet.add (accFreeze, node), 
                                                                            NodePairSet.addList (accMoves, movesInvolvedWith), 
                                                                            NodeMap.insert (accMoveMap, node, movesInvolvedWith))
                      else (NodeSet.add (accSimp, node), accSpill, accFreeze, accMoves, accMoveMap)
                    end
              val nodeList = NodeSet.listItems (Liveness.IGraph.nodes graph)

            in
                List.foldl makeListHelper (NodeSet.empty, NodeSet.empty, NodeSet.empty, NodePairSet.empty, NodeMap.empty) nodeList
            end

          val (simplifyWorklist, spillWorklist, freezeWorklist, worklistMoves, moveList) = makeWorklists ()
          
          val spilledNodes = NodeSet.empty
          val coalescedNodes = NodeSet.empty
          val coloredNodes = NodeSet.empty
          val selectStack = []

          val coalescedMoves = NodePairSet.empty
          val constrainedMoves = NodePairSet.empty
          val frozenMoves = NodePairSet.empty
          val activeMoves = NodePairSet.empty
          
          


          fun simplify () = ()
          fun coalesce () = ()
          fun freeze () = ()
          
          fun assignColors () = initial

          fun colorMain () =
            if not (NodeSet.isEmpty simplifyWorklist) then (simplify (); colorMain ())
            else if not (NodePairSet.isEmpty worklistMoves) then (coalesce (); colorMain ())
            else if not (NodeSet.isEmpty freezeWorklist) then (freeze (); colorMain ())
            else assignColors ()


        in
          (colorMain (), [])
        end
        
        
end