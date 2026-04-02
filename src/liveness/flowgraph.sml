structure Flow =
struct
  structure Graph = MakeGraph()
  type flowgraph =
    { control: Graph.graph
    , def: Temp.temp list Graph.NodeMap.map
    , use: Temp.temp list Graph.NodeMap.map
    , ismove: bool Graph.NodeMap.map
    }

  (* Liveness analysis using iteration to a fixed point *)
  (* Returns tuple (live-in, live-out) *)
  fun livenessAnalysis fgraph =
    let
      val {control, def, use, ismove = _} = fgraph
      val rcontrol = Graph.reverse control
      fun oneIteration liveSets =
        let
          fun liveSetLookup (liveSet, node) =
            case Graph.NodeMap.find (liveSet, node) of
              SOME s => s
            | NONE => Temp.Set.empty
          fun livenessAtNode ((liveIn, liveOut), node) =
            let
              (* Compute liveOut for this node *)
              (* liveOut is the union of liveIns of the successors *)
              val nodeLiveOut = liveSetLookup (liveOut, node)
              val nodeSuccs = Graph.NodeSet.toList (Graph.succ (control, node))
              val succLiveIns =
                List.map (fn n => liveSetLookup (liveIn, n)) nodeSuccs
              val nodeLiveOut = foldl Temp.Set.union nodeLiveOut succLiveIns
              val liveOut = Graph.NodeMap.insert (liveOut, node, nodeLiveOut)

              (* Compute liveIn for this node *)
              (* liveIn = liveOut - defs + uses *)
              val nodeDefs = Graph.NodeMap.lookup (def, node)
              val generated = Temp.Set.fromList nodeDefs
              val nodeUses = Graph.NodeMap.lookup (use, node)
              val killed = Temp.Set.fromList nodeUses
              val liveOutMinusKilled = Temp.Set.difference (nodeLiveOut, killed)
              val nodeLiveIn = Temp.Set.union (generated, liveOutMinusKilled)
              val liveIn = Graph.NodeMap.insert (liveIn, node, nodeLiveIn)
            in
              (liveIn, liveOut)
            end
          val sinks = Graph.NodeSet.toList (Graph.sinks control)
        in
          (* Iterate via DFS on the reverse graph, starting from the exit points *)
          Graph.foldl_dfs livenessAtNode sinks liveSets rcontrol
        end
      (* Iterates to a fixed point to create live sets *)
      fun iterate oldLiveSets =
        let
          (* Apply an iteration; see if the sets changed *)
          val newLiveSets = oneIteration oldLiveSets

          (* All this machinery required to compare the maps *)
          fun listEq ([], []) = true
            | listEq ((k1, v1) :: xs, (k2, v2) :: ys) =
                k1 = k2 andalso Temp.Set.equal (v1, v2) andalso listEq (xs, ys)
            | listEq _ = false
          fun mapEq (map1, map2) =
            listEq
              (Graph.NodeMap.listItemsi map1, Graph.NodeMap.listItemsi map2)
          fun tupleMapEq ((t11, t12), (t21, t22)) =
            mapEq (t11, t21) andalso mapEq (t12, t22)
          val eq = tupleMapEq (oldLiveSets, newLiveSets)
        in
          (* Iterate to a fixed point *)
          if eq then newLiveSets else iterate newLiveSets
        end
      (* Start with no live temps *)
      val initLiveSets = (Graph.NodeMap.empty, Graph.NodeMap.empty)
      val (liveIn, liveOut) = iterate initLiveSets
    in
      (liveIn, liveOut)
    end

(* Note:  any "use" within the block is assumed to be BEFORE a "def" 
      of the same variable.  If there is a def(x) followed by use(x)
     in the same block, do not mention the use in this data structure,
     mention only the def.

   More generally:
     If there are any nonzero number of defs, mention def(x).
     If there are any nonzero number of uses BEFORE THE FIRST DEF,
         mention use(x).

   For any node in the graph,  
         Graph.Table.look(def,node) = SOME(def-list)
         Graph.Table.look(use,node) = SOME(use-list)
 *)

end
