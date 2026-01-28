# Tests

This directory contains tests for our compiler. Many are downloaded from Andrew Appel's website linked [here](https://www.cs.princeton.edu/~appel/modern/testcases/).

## Running Existing Tests

Existing tests can be found in `test-manifest.sml`. Run them with the function `Tests.runTests <key>`, declared in `test-driver.sml`.

## Test Results

The results of the test on each file will be stored in `output/<key>`. If the output file exactly matches the file found in `expected/<key>`, the test will pass. Otherwise, it will fail. To see exactly what's different, use the `diff` terminal command on the files.

## Adding New Tests

Each test is listed as a record (find the declaration in `test-manifest.sig`). The list of tests known to the test engine is found in `TestManifest.allTests`, which is a list of test records located in `test-manifest.sml`. New tests should NOT be placed in `test-driver.sml`. Instead, opt to place the testing record or function in `test-manifest.sml`, or if sufficiently long in its own file.

The record has three entries:

- `test_name`: the name of the test. This is the argument passed to `Tests.runTests`.
- `test_dirs`: the group of input files to run this test on. This is a list of input directories. For example, `["appel-programs"]` will tell the test to run on every file in `input/appel-programs`.
- `test_function`: this is testing function. A test function takes two arguements: the path to the input file, and the path to the output file. The testing function is expected to read from the input file and output all of its results to the output file.

An example `echo` test is provided.
