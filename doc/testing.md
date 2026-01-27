# Testing

Testing can be done through the [tests/testing.sml] file. 

Currently, it's hardcoded to run the parser test.
After making `sources.cm`, run `Tests.runTest "\[name of the test, minus the .tig]"`

## Writing Tests
When writing tests for a specific system, create a new directory in [tests/test-programs/] corresponding to that system: e.g. [tests/test-programs/lexer-tests]
There will likely be a different driver program required. For instance, [tests/test-parse.sml] is a fork of [src/lexer/driver.sml] which returns the tokens as a string.

By default, [tests/testing.sml] looks for a `.txt` file with the same name in the `OUTPUT_DIRECTORY` (which is currently hardcoded to [tests/test-parse-expected-outputs]).
It will redirect the stdout (Error also prints to stdout) of `FUNCTION_TO_TEST` (currently set to `Parse.parse` in [src/lexer/driver.sml]) to a file in the `actual-outputs` directory.
Then, we will compare the expected and actual outputs using a call to `diff`. Why reinvent the wheel?

TODO: organize the directories here. Expected/Actual files should ideally follow the directory structure of the tests, but I'm not sure how having multiple possible driver functions changes this
TODO: organize driver functions by system?
