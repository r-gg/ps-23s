<h1 align="center">Task 3: Text Editor</h1>

# How to run

1. cabal clean
2. cabal install
3. cabal run task3 -- <file-path>

## Testfiles
- test-1.txt: Valid (cabal run task3 -- test-1.txt)
- test-2.txt: Syntax error (cabal run task3 -- test-2.txt)
- test-3.txt: Unbalanced curly braces (cabal run task3 -- test-3.txt)
- test-4.txt: Unbalanced braces (cabal run task3 -- test-4.txt)

## Default
If no filepath is given (cabal run task3) a file called document.txt will be generated in the current path.