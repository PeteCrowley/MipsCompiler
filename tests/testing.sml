structure Testing :> TESTING =
struct
  fun echo x = x

  val allTests =
    [{ test_name = "echo"
     , test_dirs = ["appel-programs", "lexer-programs"]
     , test_fn = echo
     }]

  structure StringOrdKey: ORD_KEY =
  struct type ord_key = string val compare = String.compare end
  structure StringOrdMap: ORD_MAP = RedBlackMapFn(StringOrdKey)

  type test_entry =
    {test_name: string, test_dirs: string list, test_fn: string -> string}
  fun runTests code =
    let
      fun insert ({test_name, test_dirs, test_fn}, map) =
        StringOrdMap.insert
          ( map
          , test_name
          , {test_name = test_name, test_dirs = test_dirs, test_fn = test_fn}
          )
      val allTestsMap: test_entry StringOrdMap.map =
        List.foldl insert StringOrdMap.empty allTests
      val testToRun = StringOrdMap.find (allTestsMap, code)
      (* Returns a tuple of (test-name, full input path of failed test) *)
      fun runTest {test_name, test_dirs, test_fn} =
        let
          val () = print ("Test " ^ test_name ^ ":\n")
          fun runAllTestsInDir dirname =
            let
              val dirstream = OS.FileSys.openDir dirname
              fun runAllRemainingTestsInDir (ds, failedTests) =
                let
                  val maybeNextFile = OS.FileSys.readDir ds
                in
                  case maybeNextFile of
                    NONE => failedTests
                  | SOME file => runTestOnFile (ds, file, failedTests)
                end
              and runTestOnFile (ds, file, failedTests) =
                let
                  val fullInputPath = "tests/input/" ^ dirname ^ "/" ^ file
                  val inputStream = TextIO.openIn fullInputPath
                  val input = TextIO.inputAll inputStream
                  val output = test_fn input
                  val thisTestSucceeded = input = output
                  val testStatusStr =
                    if thisTestSucceeded then "PASSED" else "FAILED"
                  val () = print
                    ("  on " ^ fullInputPath ^ ": " ^ testStatusStr ^ "\n")
                  val failedTests =
                    if thisTestSucceeded then failedTests
                    else (test_name, fullInputPath) :: failedTests
                in
                  runAllRemainingTestsInDir (ds, failedTests)
                end
              val failedTests = runAllRemainingTestsInDir (dirstream, [])
            in
              failedTests
            end
          val testResultsForEachDir = List.map runAllTestsInDir test_dirs
          val failedTests = List.concat testResultsForEachDir
        in
          failedTests
        end
      fun codeDoesntMatchTestName () =
        let val () = print ("No test named " ^ code ^ "\n")
        in []
        end
      val results =
        case testToRun of
          NONE => codeDoesntMatchTestName ()
        | SOME record => runTest record
      val () =
        if results = [] then print ("ALL TESTS PASSED\n")
        else print "SOME TESTS FAILED\nFailed Tests:\n"
      fun printFailure (test_name, path) =
        print ("  " ^ test_name ^ " on " ^ path ^ "\n")
      val _ = List.map printFailure results
    in
      ()
    end
end
