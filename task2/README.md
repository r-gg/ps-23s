# Task 2

## Description

An interpreter for a simple functional programming language.

### Features

1. Variable scoping (lexical) e.g. (x -> (x -> x) 1) 2 ====> 1
    Achieved by by adding "-hash" at the end of the variable name where hash consists of the first 4 characters of a random uuid
2. Higher order functions e.g. (x -> x) (y -> y) 1 ====> 1
3. Partial evaluation
4. Lazy evaluation
5. Recursion
6. Closures (Records/Environments)
7. Currying
8. Conditionals
9. Global environment
10. List example ~ functions:
    - list
    - range
    - reduce
    - sum
    - prod
    - fac

## General workflow

1. Parse input with `lark`
2. Transform the parse tree into an AST (type json) with `transformer.py`
3. Evaluate the AST with `evaluator.py`
   - firstly evaluate the global environment if present (if there is a record before the main expression)
   - then evaluate the main expression

## Setup

```bash
$ pip3 install -r requirements.txt
```

## Usage

```bash
$ python3 main.py <filename>
```

If no filename is provided, contents of `test.txt` will be taken as input. 

## Tests

```bash
$ python3 -m pytest tests.py
```


## Examples

### The original list example

#### Input

```
{
    list = c -> f -> x ->
        cond (c x) {
            val = x,
            nxt = list c f (f x)
        } {},
    reduce = f -> x -> lst ->
        cond lst (
            f (reduce f x (lst nxt)) (lst val)
        ) x,
    range = a -> b ->
        list (
            (x -> minus b x)
        ) (
            (x -> plus 1 x)
        ) a,
    sum = lst ->
        reduce (
            (x -> y -> plus x y)
        ) 0 lst
} sum (range 3 6)
```

#### Output

```
{
    list = (c -> f -> x ->
        cond (c x) {
            val = x,
            nxt = list c f (f x)
        } {}
    ),
    reduce = (f -> x -> lst ->
        cond lst (
            f (reduce f x (lst nxt)) (lst val)
        ) x
    ),
    range = (a -> b ->
        list (
            (x -> minus b x)
        ) (
            (x -> plus 1 x)
        ) a
    ),
    sum = (lst ->
        reduce (
            (x -> y -> plus x y)
        ) 0 lst
    )
} 12
```

### Using prod instead of sum

#### Input

```
{
    list = c -> f -> x ->
        cond (c x) {
            val = x,
            nxt = list c f (f x)
        } {},
    reduce = f -> x -> lst ->
        cond lst (
            f (reduce f x (lst nxt)) (lst val)
        ) x,
    range = a -> b ->
        list (
            (x -> minus b x)
        ) (
            (x -> plus 1 x)
        ) a,
    prod = lst ->
        reduce (
            (x -> y -> mult x y)
        ) 1 lst
} prod (range 1 5)
```

#### Output

```
{
    list = (c -> f -> x ->
        cond (c x) {
            val = x,
            nxt = list c f (f x)
        } {}
    ),
    reduce = (f -> x -> lst ->
        cond lst (
            f (reduce f x (lst nxt)) (lst val)
        ) x
    ),
    range = (a -> b ->
        list (
            (x -> minus b x)
        ) (
            (x -> plus 1 x)
        ) a
    ),
    prod = (lst ->
        reduce (
            (x -> y -> mult x y)
        ) 1 lst
    )
} 24
```

### Factorial added

#### Input

```
{
    list = c -> f -> x ->
        cond (c x) {
            val = x,
            nxt = list c f (f x)
        } {},
    reduce = f -> x -> lst ->
        cond lst (
            f (reduce f x (lst nxt)) (lst val)
        ) x,
    range = a -> b ->
        list (
            (x -> minus b x)
        ) (
            (x -> plus 1 x)
        ) a,
    prod = lst ->
        reduce (
            (x -> y -> mult x y)
        ) 1 lst
} {
    fac = n -> prod (range 1 n)
} fac 5
```

#### Output

```
{
    list = (c -> f -> x ->
        cond (c x) {
            val = x,
            nxt = list c f (f x)
        } {}
    ),
    reduce = (f -> x -> lst ->
        cond lst (
            f (reduce f x (lst nxt)) (lst val)
        ) x
    ),
    range = (a -> b ->
        list (
            (x -> minus b x)
        ) (
            (x -> plus 1 x)
        ) a
    ),
    prod = (lst -> 
        reduce (
            (x -> y -> mult x y)
        ) 1 lst
    )
} 24
```

**Note: More examples are in `tests.py`**