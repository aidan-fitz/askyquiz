# Contributing Guide

We'll document our agreed-upon practices for contributing to this project here.

## Test Suite

All of us should be familiar with the contents of the test suite. We expect that
doing this will help reduce internal resistance to writing tests. To make
understanding the test suite easier, we have structured it as a set of several
files each testing specific functionality.

Files that match the glob `test*.ml` are test files. Every program file (module)
should have a corresponding test file.

## Build System

Like previous course assignments, our project use OCamlbuild and a Makefile.

We have modified the default course Makefile to keep program and test files
logically separate. `PROGRAM_MODULES` contains the names of program files
(without extensions); `TEST_MODULES` contains the names of test files. These
lists are concatenated together as the `MODULES` variable.

## VS Code Settings

`.vscode/settings.json` is configured to apply JSON syntax highlighting to files
ending in `.quiz`. Normally, we'd put the `.vscode` in our global gitignore
settings, but we make an exception for this project because it makes editing
`.quiz` files easier. Bear in mind that changes to this file will affect the
VS Code settings for the entire team.
