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

  structure Set: ORD_SET where type Key.ord_key = temp
  structure Map: ORD_MAP where type Key.ord_key = temp
end
