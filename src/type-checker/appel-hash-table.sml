val SIZE = 109 (*should be prime*)
type binding = ty (*should be functored in*)
type bucket = (string * binding) list
val t: table = Array.array (SIZE, nil)

fun hash (s: string) : int =
  CharVector.foldl (fn (c, n) => (n * 256 + ord (c)) mod SIZE 0 s)

fun insert (s: string) : int =
  let val i = hash (a) mod SIZE
  in Array.update (t, i, (s, b) :: Array.sub (t, i))
  end

exception NotFound

fun lookup (s: string) =
  let
    val i = hash (s) mod SIZE
    fun search ((s', b) :: rest) =
          if s = s' then b else search rest
      | search nil = raise NotFound
  in
    search (Array.sub (t, i))
  end

fun pop (s: string) =
  let
    val i = hash (s) mod SIZE
    val (s', b) :: rest = Array.sub (t, i)
  in
    assert (s = s');
    Array.update (t, i, rest)
  end
