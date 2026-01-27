structure Tests =
struct
  val allTests =
    [ "merge"
    , "queens"
    , "test01"
    , "test02"
    , "test03"
    , "test04"
    , "test05"
    , "test06"
    , "test07"
    , "test08"
    , "test09"
    , "test10"
    , "test11"
    , "test12"
    , "test13"
    , "test14"
    , "test15"
    , "test16"
    , "test17"
    , "test18"
    , "test19"
    , "test20"
    , "test21"
    , "test22"
    , "test23"
    , "test24"
    , "test25"
    , "test26"
    , "test27"
    , "test28"
    , "test29"
    , "test30"
    , "test31"
    , "test32"
    , "test33"
    , "test34"
    , "test35"
    , "test36"
    , "test37"
    , "test38"
    , "test39"
    , "test40"
    , "test41"
    , "test42"
    , "test43"
    , "test44"
    , "test45"
    , "test46"
    , "test47"
    , "test48"
    , "test49"
    ]

  val PROGRAM_TO_TEST = Parse.parse
  (* Not exactly sure what this will look like should be a function that takes some input and output *)

  val PREFIX_DIRECTORY = "tests"
  val INPUT_DIRECTORY = PREFIX_DIRECTORY ^ "/test-programs" ^ "/lexer-tests"
  val INPUT_EXTENSION = ".tig"
  val EXPECTED_OUT_DIRECTORY =
    PREFIX_DIRECTORY ^ "/" ^ "test-parse-expected-outputs"
  val EXPECTED_OUT_EXTENSION = ".txt"
  val OUTPUT_DIRECTORY = PREFIX_DIRECTORY ^ "/" ^ "test-parse-actual-outputs"
  val OUTPUT_EXTENSION = ".out"

  fun runTest testName =
    let
      (* val inputStream = TextIO.openIn *)
      (*   (INPUT_DIRECTORY ^ "/" ^ testName ^ INPUT_EXTENSION) *)
      (* val input = TextIO.inputAll inputStream *)
      (* val () = TextIO.closeIn inputStream *)

      val expectedFileName =
        (EXPECTED_OUT_DIRECTORY ^ "/" ^ testName ^ EXPECTED_OUT_EXTENSION)
      val expectedOutputStream = TextIO.openIn expectedFileName
      (* val expectedOutput = TextIO.inputAll expectedOutputStream *)
      val () = TextIO.closeIn expectedOutputStream

      (* redirect stdout output to output directory *)
      val outputFileName = OUTPUT_DIRECTORY ^ "/" ^ testName ^ OUTPUT_EXTENSION
      val () = IOUtil.withOutputFile (outputFileName, PROGRAM_TO_TEST)
        (INPUT_DIRECTORY ^ "/" ^ testName ^ INPUT_EXTENSION)

    in
      (* There already exists a nice tool to help us `diff` files. Let's just call it. *)
      OS.Process.isSuccess (OS.Process.system
        ("diff " ^ expectedFileName ^ " " ^ outputFileName))
    end

  fun runAllTests testList =
    let
      val numPassed =
        foldl (fn (test, acc) => if runTest test then acc + 1 else acc) 0
          testList
      val numTests = List.length testList
      val () = print
        ("**** Tests Finished Running ***\nResults: " ^ Int.toString numPassed
         ^ "/" ^ Int.toString numTests ^ " tests passed")
    in
      ()
    end
end
