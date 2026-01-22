structure Tests = 
struct
val allTests = [
    "merge",
    "queens",
    "test1",
    "test2",
    "test3",
    "test4",
    "test5",
    "test6",
    "test7",
    "test8",
    "test9",
    "test10",
    "test11",
    "test12",
    "test13",
    "test14",
    "test15",
    "test16",
    "test17",
    "test18",
    "test19",
    "test20",
    "test21",
    "test22",
    "test23",
    "test24",
    "test25",
    "test26",
    "test27",
    "test28",
    "test29",
    "test30",
    "test31",
    "test32",
    "test33",
    "test34",
    "test35",
    "test36",
    "test37",
    "test38",
    "test39",
    "test40",
    "test41",
    "test42",
    "test43",
    "test44",
    "test45",
    "test46",
    "test47",
    "test48",
    "test49"
]

val PROGRAM_TO_TEST = Echo.echo
(* Not exactly sure what this will look like should be a function that takes some input and output *)

val PREFIX_DIRECTORY = "tests"
val INPUT_DIRECTORY = PREFIX_DIRECTORY ^ "/" ^ "test-programs"
val INPUT_EXTENSION = ".tig"
val OUTPUT_DIRECTORY = PREFIX_DIRECTORY ^ "/" ^ "echo-expected-outputs"
val OUTPUT_EXTENSION = ".txt"


fun runTest testName = 
    let
        val inputStream = TextIO.openIn (INPUT_DIRECTORY ^ "/" ^testName ^ INPUT_EXTENSION)
        val input = TextIO.inputAll inputStream
        val () = TextIO.closeIn inputStream

        val expectedOutputStream = TextIO.openIn(OUTPUT_DIRECTORY ^ "/" ^ testName ^ OUTPUT_EXTENSION)
        val expectedOutput = TextIO.inputAll expectedOutputStream
        val () = TextIO.closeIn expectedOutputStream

        val programOutput = PROGRAM_TO_TEST input

        val areEqual = expectedOutput = programOutput
        val () = if not areEqual then print("Test " ^ testName ^ " Failed: Expected:\n" ^ expectedOutput ^ 
                                            "\nBut got:\n" ^ programOutput)
                    else print("Test " ^ testName ^ " Passed\n")

    in
        areEqual
    end

fun runAllTests testList =
    let
        val numPassed = foldl (fn (test, acc) => if runTest test then acc + 1 else acc) 0 testList
        val numTests = List.length testList
        val () = print("**** Tests Finished Running ***\nResults: " ^ Int.toString numPassed ^ 
                        "/" ^ Int.toString numTests ^ " tests passed")
    in
        ()
    end
end