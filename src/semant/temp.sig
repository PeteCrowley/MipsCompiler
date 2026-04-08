signature TEMP =
sig
  eqtype temp
  val newtemp: unit -> temp
  structure Table: TABLE
  sharing type Table.key = temp
  val makestring: temp -> string
  type label = Symbol.symbol
  val newlabel: unit -> label
  val namedlabel: string -> label
  val reset: unit -> unit

  (* TODO: switch to exporting either Key or Set and Map (not both) *)
  structure Key: ORD_KEY where type ord_key = temp
  structure Set: ORD_SET where type Key.ord_key = temp
  structure Map: ORD_MAP where type Key.ord_key = temp
end
