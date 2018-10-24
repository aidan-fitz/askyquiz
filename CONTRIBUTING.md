# Contributing Guide

We'll document our agreed-upon practices for contributing to this project here.

## Test Suite

All of us should be familiar with the contents of the test suite. We expect that
doing this will help reduce internal resistance to writing tests. To make
understanding the test suite easier, we have structured it as a set of several
files each testing specific functionality.

Files that match the glob `test*.ml` are test files. Every program file (module)
should have a corresponding test file.

To create a new test file:

1. Save the file
2. Add the filename without the `.ml` extension to the `MODULES` variable in
   `Makefile` (FIXME we might factor this out to a different variable called,
   say, `TEST_MODULES`)
