# Task 2

## Description

An interpreter for a simple functional programming language.

### Features

1. Variable scoping (lexical) e.g. (x -> (x -> x) 1) 2 ====> 1
    Achieved by by adding "---i" at the end of the variable name where i is the number of times the variable has been used in the current scope.
2. Higher order functions e.g. (x -> x) (y -> y) 1 ====> 1
3. Partial evaluation
4. Lazy evaluation
5. Recursion
6. Closures (Records/Environments)
7. Currying

### TODOs

- Remove global env.

## Usage

```bash
$ python3 main.py <filename>
```

## Setup

```bash
$ pip3 install -r requirements.txt
```

## Tests

```bash
$ python3 -m pytest tests.py
```