# Tests
This directory contains tests for our compiler. Many are downloaded from Andrew Appel's website linked [here](https://www.cs.princeton.edu/~appel/modern/testcases/).

### Testing Setup
Expected tests results should be placed in the appropriate directory for that layer of the compiler. For example, the expected output of my example `echo` program is defined in `echo-expected-outputs`. Then, configure a few variables in the `testing.sml` file. 

1. Set the `INPUT_DIRECTORY` and `INPUT_EXTENSION` variables. These (I think) should often just be `test-programs` and `.tig` but there may be some cases we don't want to test all layers of our pipleine and instead want to start at some intermediate step (say an AST). 
2. Set the `OUTPUT_DIRECTORY` and `OUTPUT_EXTENSION` variables based on where you've placed the expected outputs. 
3. Set the `PROGRAM_TO_TEST` variable with the program you'd like to test. Right now, the testing framework will just call this function with a single string input but this can be easily extended in the future.
4. If you only want to run certain tests, comment out unwanted ones in the `allTests` list.

### Running Tests
To run tests, you can use the `Test` structure defined in the `testing.sml` file. Simply make the `sources.cm` file as normal and then call `Tests.runAllTests Tests.allTests` to run all tests or `Tests.runTest <testName>` to run a singular test.