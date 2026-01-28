structure Tests :> TESTS =
struct
  structure StringOrdKey: ORD_KEY =
  struct type ord_key = string val compare = String.compare end
  structure StringOrdMap: ORD_MAP = RedBlackMapFn(StringOrdKey)

  fun ensureDir path =
    let
      val path = OS.Path.mkCanonical path

      fun existsDir p =
        OS.FileSys.isDir p
        handle OS.SysErr _ => false

      fun mkdir p =
        OS.FileSys.mkDir p
        handle OS.SysErr _ => () (* Ignore EEXIST *)

      fun ensure p =
        if p = "" orelse p = OS.Path.currentArc then
          ()
        else if existsDir p then
          ()
        else
          let val parent = OS.Path.dir p
          in if parent <> p then ensure parent else (); mkdir p
          end
    in
      ensure path
    end

  fun readFile path =
    let
      val stream = TextIO.openIn path
      val contents = TextIO.inputAll stream
      val () = TextIO.closeIn stream
    in
      contents
    end

  fun tryReadFile path =
    readFile path
    handle IO.Io _ => (TextIO.print ("Could not find " ^ path ^ "\n"); "")


  fun runTests code =
    let
      fun insert ({test_name, test_dirs, test_fn}, map) =
        StringOrdMap.insert
          ( map
          , test_name
          , {test_name = test_name, test_dirs = test_dirs, test_fn = test_fn}
          )
      val allTestsMap: TestManifest.test_entry StringOrdMap.map =
        List.foldl insert StringOrdMap.empty TestManifest.allTests
      val testToRun = StringOrdMap.find (allTestsMap, code)
      (* Returns a tuple of (test-name, full input path of failed test) *)
      fun runTest {test_name, test_dirs, test_fn} =
        let
          val () = print ("Test " ^ test_name ^ ":\n")
          fun runAllTestsInDir dirname =
            let
              val dirstream = OS.FileSys.openDir ("tests/input/" ^ dirname)
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
                  (* Open input/output FDs and run the test function*)
                  val fullInputPath = "tests/input/" ^ dirname ^ "/" ^ file
                  val fullOutputDir =
                    "tests/output/" ^ test_name ^ "/" ^ dirname ^ "/"
                  val () = ensureDir fullOutputDir
                  val fullOutputPath = fullOutputDir ^ file ^ ".txt"

                  (* Run the test *)
                  val () = test_fn (fullInputPath, fullOutputPath)

                  (* Read the expected and actual output file *)
                  val fullExpectedPath =
                    "tests/expected/" ^ test_name ^ "/" ^ dirname ^ "/" ^ file
                    ^ ".txt"
                  val expected = tryReadFile fullExpectedPath
                  val actual = readFile fullOutputPath

                  (* Adjudicate the output *)
                  val testSucceeded = expected = actual
                  val testStatusStr =
                    if testSucceeded then "PASSED" else "FAILED"
                  val () = print
                    ("  on " ^ fullInputPath ^ ": " ^ testStatusStr ^ "\n")
                  val failedTests =
                    if testSucceeded then failedTests
                    else (test_name, fullInputPath) :: failedTests
                in
                  runAllRemainingTestsInDir (ds, failedTests)
                end
            in
              runAllRemainingTestsInDir (dirstream, [])
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
