structure MakeGraph:
sig
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end =
struct
  fun instrs2graph insns =
    let
      (* Assemble the nodes of the graph *)
      fun mapInstrs ([], graph, labelMap) = ([], graph, labelMap)
        | mapInstrs (instr :: instrs, graph, labelMap) =
            let
              val {control, def, use, ismove} = graph

              (* Create a new node for this instruction and add it to the graph *)
              val node = Flow.Graph.find_unused_node control
              val control = Flow.Graph.addNode (control, node)

              (* Populate the other graph fields; capture jump targets *)
              val (graph, jump, labelMap) =
                case instr of
                  Assem.OPER {assem = _, dst, src, jump} =>
                    let
                      val def = Flow.Graph.NodeMap.insert (def, node, dst)
                      val use = Flow.Graph.NodeMap.insert (use, node, src)
                      val ismove =
                        Flow.Graph.NodeMap.insert (ismove, node, false)
                    in
                      ( { control = control
                        , def = def
                        , use = use
                        , ismove = ismove
                        }
                      , jump
                      , labelMap
                      )
                    end
                | Assem.LABEL {assem = _, lab} =>
                    let
                      val control = Flow.Graph.addNode (control, node)
                      val labelMap = Symbol.enter (labelMap, lab, node)
                    in
                      ( { control = control
                        , def = def
                        , use = use
                        , ismove = ismove
                        }
                      , NONE
                      , labelMap
                      )
                    end
                | Assem.MOVE {assem = _, dst, src} =>
                    let
                      val control = Flow.Graph.addNode (control, node)
                      val def = Flow.Graph.NodeMap.insert (def, node, [dst])
                      val use = Flow.Graph.NodeMap.insert (use, node, [src])
                      val ismove =
                        Flow.Graph.NodeMap.insert (ismove, node, false)
                    in
                      ( { control = control
                        , def = def
                        , use = use
                        , ismove = ismove
                        }
                      , NONE
                      , labelMap
                      )
                    end
              (* Make the recursive call *)
              val (nodeJumps, graph, labelMap) =
                mapInstrs (instrs, graph, labelMap)

              (* If jump=NONE and there's a following node, add an edge for
               * fall-through from previous node *)
              val graph =
                case (nodeJumps, jump) of
                  ((nextNode, _) :: _, NONE) =>
                    { control =
                        Flow.Graph.addEdge
                          (#control graph, {from = node, to = nextNode})
                    , def = def
                    , use = use
                    , ismove = ismove
                    }
                | _ => graph
            in
              ((node, jump) :: nodeJumps, graph, labelMap)
            end

      (* Make call to recursive function *)
      val emptyGraph =
        { control = Flow.Graph.empty
        , def = Flow.Graph.NodeMap.empty
        , use = Flow.Graph.NodeMap.empty
        , ismove = Flow.Graph.NodeMap.empty
        }
      val (nodeJumps, graph, labelMap) =
        mapInstrs (insns, emptyGraph, Symbol.empty)

      (* Add jumping edges *)
      fun addJumpingEdges ((node, NONE), control) = control
        | addJumpingEdges ((node, SOME jump), control) =
            let
              fun addJumpingEdge (target, c) =
                let
                  val targetNode =
                    case Symbol.look (labelMap, target) of
                      SOME n => n
                    | NONE =>
                        raise Fail ("Unknown jump target " ^ Symbol.name target)
                in
                  Flow.Graph.addEdge (c, {from = node, to = targetNode})
                end
            in
              foldl addJumpingEdge control jump
            end
      val {control, def, use, ismove} = graph
      val control = foldl addJumpingEdges control nodeJumps
      val graph = {control = control, def = def, use = use, ismove = ismove}
      val nodes = List.map (fn (node, _) => node) nodeJumps
    in
      (graph, nodes)
    end

end
