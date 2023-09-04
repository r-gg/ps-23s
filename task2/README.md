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
- Problem:
  - calling list and cond is always not evaluatable
    - arguments f and x lost in reduce = a->b-> list c~(x->minus b x) f~(x->plus 1 x) x~a
      - solution: Store the arguments in the cond dict : {"type" : "core_fn", "some_params": 
  - Maybe add inherited_vars to eval_expression parameters, so that inner expressions know what vars are bound in that context. right now it is passed only in x->y->z-> ... case 
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