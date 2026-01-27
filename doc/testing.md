# Testing

Testing can be done through the [tests/testing.sml] file. 

Currently, it's hardcoded to run the parser test, and has been hijacked to take in filestreams as input.
After making `sources.cm`, run `Tests.runTest "\[name of the test, minus the .tig extension]"`

## Writing Tests
When writing tests for a specific system, create a new directory in [tests/test-programs/] corresponding to that system: e.g. [tests/test-programs/lexer-tests]
There will likely be a different driver program required. For instance, [tests/test-parse.sml] is a fork of [src/lexer/driver.sml] which returns the tokens as a string.

By default, [tests/testing.sml] looks for a `.txt` file with the same name in the `OUTPUT_DIRECTORY` (which is currently hardcoded to [tests/test-parse-expected-outputs]).
It will compare the output of `FUNCTION_TO_TEST` (currently set to [tests/test-parse.sml]) to the plaintext in the `.txt` file, and print out a line-by-line diff of the expected and actual outputs.

TODO: organize the directories here. Output files should ideally follow the directory structure of the tests, but I'm not sure how having multiple possible driver functions changes this
TODO: organize driver functions by system?
