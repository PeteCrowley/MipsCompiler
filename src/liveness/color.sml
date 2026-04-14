structure Color: COLOR =
struct
  structure Frame = MipsFrame
  type allocation = Frame.register Temp.Table.table
  structure NodeSet = Liveness.IGraph.NodeSet
  structure NodeMap = Liveness.IGraph.NodeMap

  structure NodePairOrd =
  struct
    type ord_key = Liveness.IGraph.node * Liveness.IGraph.node
    fun compare ((a1, b1), (a2, b2)) =
      case NodeSet.Key.compare (a1, a2) of
        EQUAL => NodeSet.Key.compare (b1, b2)
      | other => other
  end
  structure NodePairSet = RedBlackSetFn(NodePairOrd)


  fun color {interference, initial, spillCost, registers, prefRegOrder} =
    let
      val K = List.length registers
      val Liveness.IGRAPH {graph, tnode, gtemp, moves} = interference
      fun degree (node, igraph) =
        NodeSet.numItems (Liveness.IGraph.adj (igraph, node))
      val okColors =
        case prefRegOrder of
          SOME lst => lst
        | NONE => List.tabulate (K, fn i => i)

      fun sayNode n =
        Liveness.IGraph.dbg_nodename n ^ " (" ^ Temp.makestring (gtemp n) ^ ")"

      val precoloredNodes =
        let
          val allNodes = Liveness.IGraph.nodes graph
          fun isPrecolored node =
            case Temp.Table.look (initial, gtemp node) of
              SOME _ => true
            | NONE => false
        in
          NodeSet.fromList
            (List.filter isPrecolored (NodeSet.listItems allNodes))
        end

      fun makeWorklists () =
        let
          fun makeListHelper
            (node, (accSimp, accSpill, accFreeze, accMoves, accMoveMap)) =
            let
              val movesInvolvedWith = NodePairSet.fromList
                (List.filter (fn (u, v) => u = node orelse v = node) moves)
              val (newMoves, newMoveMap) =
                if not (NodePairSet.isEmpty movesInvolvedWith) then
                  ( NodePairSet.union (accMoves, movesInvolvedWith)
                  , NodeMap.insert (accMoveMap, node, movesInvolvedWith)
                  )
                else
                  (accMoves, accMoveMap)

            in
              if degree (node, graph) >= K then
                ( accSimp
                , NodeSet.add (accSpill, node)
                , accFreeze
                , newMoves
                , newMoveMap
                )
              else if not (NodePairSet.isEmpty movesInvolvedWith) then
                ( accSimp
                , accSpill
                , NodeSet.add (accFreeze, node)
                , newMoves
                , newMoveMap
                )
              else
                ( NodeSet.add (accSimp, node)
                , accSpill
                , accFreeze
                , newMoves
                , newMoveMap
                )
            end
          val nonPrecoloredNodes =
            NodeSet.filter
              (fn node => not (NodeSet.member (precoloredNodes, node)))
              (Liveness.IGraph.nodes graph)

        in
          NodeSet.foldl makeListHelper
            ( NodeSet.empty
            , NodeSet.empty
            , NodeSet.empty
            , NodePairSet.empty
            , NodeMap.empty
            ) nonPrecoloredNodes
        end

      val (simplifyWl, spillWl, freezeWl, wlMoves, moveList) = makeWorklists ()
      fun printList [] = ""
        | printList (x :: xs) =
            sayNode x ^ " " ^ printList xs

      fun colorMain
        ( igraph
        , simplifyWorklist
        , spillWorklist
        , freezeWorklist
        , coalescedNodes
        , selectStack
        , activeMoves
        , worklistMoves
        , alias
        ) =
        let
          fun nodeMoves (node, activeMovs, worklistMovs) =
            let
              val moves =
                case NodeMap.find (moveList, node) of
                  SOME movs =>
                    NodePairSet.intersection
                      (movs, NodePairSet.union (activeMovs, worklistMovs))
                | NONE => NodePairSet.empty
            (* val () =
              print
                ("nodeMoves(" ^ sayNode node ^ ") -> "
                 ^ Int.toString (NodePairSet.numItems moves) ^ "\n") *)
            in
              moves
            end

          fun isMoveRelated (node, activeMovs, worklistMovs) =
            let
              val related = not (NodePairSet.isEmpty
                (nodeMoves (node, activeMovs, worklistMovs)))
            (* val () =
              print
                ("isMoveRelated(" ^ sayNode node ^ ") -> "
                 ^ Bool.toString related ^ "\n") *)
            in
              related
            end


          fun removeNodeFromGraph node =
            let
              val adjNodes = Liveness.IGraph.adj (graph, node)

              fun enableMoves nodes =
                let

                  fun enableMove (m, (accActive, accWorklist)) =
                    if NodePairSet.member (accActive, m) then
                      ( NodePairSet.delete (accActive, m)
                      , NodePairSet.add (accWorklist, m)
                      )
                    else
                      (accActive, accWorklist)

                  fun enableMovesForNode (adjNode, (accActive, accWorklist)) =
                    let
                      (* val () = print ("enabling moves for node " ^ sayNode adjNode ^ "\n") *)
                      val moves = nodeMoves (adjNode, accActive, accWorklist)
                    in
                      NodePairSet.foldl enableMove (accActive, accWorklist)
                        moves
                    end
                in
                  NodeSet.foldl enableMovesForNode (activeMoves, worklistMoves)
                    nodes
                end

              fun updateWorklist (adjNode, (simplify, spill, freeze)) =
                if
                  degree (adjNode, igraph) = K
                  andalso not (NodeSet.member (precoloredNodes, adjNode))
                then
                  let
                    (* val () = print ("node " ^ sayNode adjNode ^ " now has degree " ^ Int.toString (degree (adjNode, igraph)) ^ "\n") *)
                    val (newActive, newWorklist) = enableMoves (NodeSet.union
                      ( NodeSet.singleton adjNode
                      , Liveness.IGraph.adj (igraph, adjNode)
                      ))
                  (* val () = print ("enabled moves for " ^ sayNode adjNode ^ "\n") *)
                  in
                    if isMoveRelated (adjNode, newActive, newWorklist) then
                      ( simplify
                      , NodeSet.delete (spill, adjNode)
                      , NodeSet.add (freeze, adjNode)
                      )
                    else
                      ( NodeSet.add (simplify, adjNode)
                      , NodeSet.delete (spill, adjNode)
                      , freeze
                      )
                  end
                else
                  (simplify, spill, freeze)
              val (newSimplify, newSpill, newFreeze) =
                NodeSet.foldl updateWorklist
                  (simplifyWorklist, spillWorklist, freezeWorklist) adjNodes
              (* val () = print ("Updated freeze worklist: " ^ printList (NodeSet.toList newFreeze) ^ "\n")
              val () = print ("Updated spill worklist: " ^ printList (NodeSet.toList newSpill) ^ "\n")
              val () = print ("Updated simplify worklist: " ^ printList (NodeSet.toList newSimplify) ^ "\n") *)
              val newGraph = Liveness.IGraph.rmNode (igraph, node)

            in
              (newGraph, newSimplify, newSpill, newFreeze)
            end

          fun simplify () =
            let
              val node = NodeSet.minItem simplifyWorklist
              val (newGraph, newSimplify, newSpill, newFreeze) =
                removeNodeFromGraph node
              val newSimplifyWorklist = NodeSet.delete (newSimplify, node)
            in
              colorMain
                ( newGraph
                , newSimplifyWorklist
                , newSpill
                , newFreeze
                , coalescedNodes
                , node :: selectStack
                , activeMoves
                , worklistMoves
                , alias
                )
            end

          fun getAlias node =
            case NodeMap.find (alias, node) of
              SOME n => getAlias n
            | NONE => node

          fun coalesce () =
            let
              (* val () = print ("freeze worklist: " ^ printList (NodeSet.toList freezeWorklist) ^ "\n") *)
              val (x, y) = NodePairSet.minItem worklistMoves
              val (alx, aly) = (getAlias x, getAlias y)
              val (u, v) =
                if NodeSet.member (precoloredNodes, aly) then (aly, alx)
                else (alx, aly)
              (* val () = print ("attempting to coalesce " ^ sayNode u ^ " and " ^ sayNode v ^ "\n") *)
              val newWorklistMoves = NodePairSet.delete (worklistMoves, (x, y))
              (* TODO: will update these later to actually coalesce stuff but will just say we can't do any for now*)
              val (newSimplify, newFreeze, newActiveMoves) =
                ( simplifyWorklist
                , freezeWorklist
                , NodePairSet.add (activeMoves, (x, y))
                )
            in
              colorMain
                ( igraph
                , newSimplify
                , spillWorklist
                , newFreeze
                , coalescedNodes
                , selectStack
                , newActiveMoves
                , newWorklistMoves
                , alias
                )
            end

          fun freezeMoves u =
            let
              fun freezeMove ((x, y), (simpAcc, freezeAcc, activeMovesAcc)) =
                let
                  val v =
                    if getAlias y = getAlias u then getAlias x else getAlias y
                  (* val () = print ("freezing move (" ^ sayNode x ^ ", " ^ sayNode y ^ ")\n") *)
                  val newActiveMoves =
                    NodePairSet.delete (activeMovesAcc, (x, y))
                  (* val () = print ("spill worklist before freezing moves: " ^ printList (NodeSet.toList spillWorklist) ^ "\n")
                  val () = print ("simplify worklist before freezing moves: " ^ printList (NodeSet.toList simpAcc) ^ "\n")
                  val () = print ("freeze worklist before freezing moves: " ^ printList (NodeSet.toList freezeAcc) ^ "\n")
                  val () = print ("v : " ^ sayNode v ^ "\n")
                  val () = print ("degree of v: " ^ Int.toString (degree (v, igraph)) ^ "\n") *)
                  val (newFreeze, newSimplify) =
                    if
                      not (NodeSet.member (precoloredNodes, v))
                      andalso
                      not (isMoveRelated (v, activeMovesAcc, worklistMoves))
                      andalso degree (v, igraph) < K
                    then
                      (NodeSet.delete (freezeAcc, v), NodeSet.add (simpAcc, v))
                    else
                      (freezeAcc, simpAcc)
                in
                  (newSimplify, newFreeze, newActiveMoves)
                end

              val moves =
                case NodeMap.find (moveList, u) of
                  SOME movs => NodePairSet.intersection (movs, activeMoves)
                | NONE => NodePairSet.empty

            in
              NodePairSet.foldl freezeMove
                (simplifyWorklist, freezeWorklist, activeMoves) moves
            end

          fun freeze () =
            let
              val u = NodeSet.minItem freezeWorklist
              (* val () = print ("freezing node " ^ Temp.makestring (gtemp u) ^ "\n") *)
              val (newSimplify, newFreeze, newActiveMoves) = freezeMoves u
              (* val () = print ("simplify worklist after freezing moves\n") *)
              val newFreezeWorklist = NodeSet.delete (newFreeze, u)
              val newSimplifyWorkList = NodeSet.add (newSimplify, u)
            in
              colorMain
                ( igraph
                , newSimplifyWorkList
                , spillWorklist
                , newFreezeWorklist
                , coalescedNodes
                , selectStack
                , newActiveMoves
                , worklistMoves
                , alias
                )
            end

          fun spill () =
            let
            (* val () = print ("Spill worklist " ^ printList (NodeSet.toList spillWorklist) ^ "\n") *)
            in
              (* raise Fail "Spill not implemented" *)
              colorMain
                ( igraph
                , simplifyWorklist
                , NodeSet.delete (spillWorklist, NodeSet.minItem spillWorklist)
                , freezeWorklist
                , coalescedNodes
                , selectStack
                , activeMoves
                , worklistMoves
                , alias
                )
            end

          fun assignColors () =
            let
              fun assignHelper ([], colorMap) = colorMap
                | assignHelper (node :: rest, colorMap) =
                    let
                      fun getAvailableColorsHelper (adjNode, openColors) =
                        let
                          val aliasNode = getAlias adjNode
                        in
                          case NodeMap.find (colorMap, aliasNode) of
                            SOME clr =>
                              List.filter (fn c => c <> clr) openColors
                          | NONE => openColors
                        end

                      val availableColors =
                        NodeSet.foldl getAvailableColorsHelper okColors
                          (Liveness.IGraph.adj (graph, node))
                      (* TODO: could do more logic to choose a certain register more often than others *)
                      val chosenColor =
                        case availableColors of
                          [] => raise Fail "Spill required"
                        | c :: _ => c
                      val newColorMap =
                        NodeMap.insert (colorMap, node, chosenColor)
                    in
                      assignHelper (rest, newColorMap)
                    end
              val initialColorMap =
                NodeSet.foldl
                  (fn (node, acc) => NodeMap.insert (acc, node, gtemp node))
                  NodeMap.empty precoloredNodes
              val colorMap = assignHelper (selectStack, initialColorMap)
              val colorMapWithCoalesced =
                NodeSet.foldl
                  (fn (node, acc) =>
                     NodeMap.insert
                       ( acc
                       , node
                       , case NodeMap.find (colorMap, getAlias node) of
                           SOME col => col
                         | NONE => raise Fail "Alias of node not colored"
                       )) colorMap coalescedNodes
              val tempMappings =
                let
                  fun getTempMapping (node, acc) =
                    let
                      val temp = gtemp node
                      val reg = List.nth
                        ( registers
                        , case NodeMap.find (colorMapWithCoalesced, node) of
                            SOME col => col
                          | NONE => raise Fail "Node not colored"
                        )
                    in
                      Temp.Table.enter (acc, temp, reg)
                    end
                in
                  foldl getTempMapping Temp.Table.empty
                    (NodeMap.listKeys colorMapWithCoalesced)
                end
            in
              tempMappings
            end
        in
          if not (NodeSet.isEmpty simplifyWorklist) then simplify ()
          else if not (NodePairSet.isEmpty worklistMoves) then coalesce ()
          else if not (NodeSet.isEmpty freezeWorklist) then freeze ()
          else if not (NodeSet.isEmpty spillWorklist) then spill ()
          else assignColors ()
        end

      (* val () = print (Liveness.IGraph.dbg_dump graph)
      val () = print ("precolored: " ^ printList (NodeSet.toList precoloredNodes) ^ "\n")
      val () = print ("simplify: " ^ printList (NodeSet.toList simplifyWl) ^ "\n")
      val () = print ("spill: " ^ printList (NodeSet.toList spillWl) ^ "\n")
      val () = print ("freeze: " ^ printList (NodeSet.toList freezeWl) ^ "\n") *)

      val spilledNodes = NodeSet.empty
      val coalescedNodes = NodeSet.empty
      val coloredNodes = NodeSet.empty
      val selectStack = []

      val coalescedMoves = NodePairSet.empty
      val constrainedMoves = NodePairSet.empty
      val frozenMoves = NodePairSet.empty
      val activeMoves = NodePairSet.empty

      val alias = NodeMap.empty

    in
      ( colorMain
          ( graph
          , simplifyWl
          , spillWl
          , freezeWl
          , coalescedNodes
          , selectStack
          , activeMoves
          , wlMoves
          , alias
          )
      , []
      )
    end


end
