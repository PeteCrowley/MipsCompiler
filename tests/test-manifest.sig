signature TEST_MANIFEST =
sig
  type test_entry =
    { test_name: string
    , test_dirs: string list
    , test_fn: string * string -> unit
    }
  val allTests: test_entry list
end
