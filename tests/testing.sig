signature TESTING =
sig
  type test_entry =
    {test_name: string, test_dirs: string list, test_fn: string -> string}
  val allTests: test_entry list
  val runTests: string -> unit
end
