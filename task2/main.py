# This is a sample Python script.

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

from lark import Lark, Transformer, v_args, Tree, Token

import numpy as np
from elements import *

@v_args(inline=True)
class EvaluateExpr:
    def __init__(self):
        self.vars = {
            "mult" : lambda x, y: x * y,
            "div" : lambda x, y: x / y,
            "plus" : lambda x, y: x + y,
            "minus" : lambda x, y: x - y,
            "cond" : lambda x, y, z: y if x else z
        }



    def apply_fn(self, name):
        print("apply_fn: ", name)

    def apply_fn_multiple(self, fn, *args):
        print("apply_fn_multiple: ", fn, args)

    # Assigns value to the name
    def create_pair(self, name, value):
        print("create_pair: ", name, value)


    def create_fn(self, name):
        print("create_fn: ", name)

    def create_final_pair(self, pairs, name, value):
        print("create_final_pair: ", pairs, name, value)

    def basic_int(self, value):
        print("basic_int: ", value)

    def basic_word(self, name):
        print("basic_word: ", name)

    def create_prec_expr(self, expr):
        print("create_prec_expr: ", expr)


    def create_pairs(self, pairs):
        print("create_pairs: ", pairs)

    def var(self, name):
        try:
            return self.vars[name]
        except KeyError:
            raise Exception("Variable not found: %s" % name)

fns = {
        "mult" : lambda x: lambda y: x * y,
        "div" : lambda x: lambda y: x / y,
        "plus" : lambda x: lambda y: x + y,
        "minus" : lambda x: lambda y: x - y,
        "cond" : lambda x: lambda y: lambda z: y if (x != 0) else z # instead y do eval_expr(y) and eval_expr(z) respectively
    }

functions = {
    "mult" : CoreFunction("mult", lambda_fn=lambda input: input['x'] * input['y']),
    "plus" : CoreFunction("plus", lambda_fn=lambda input: input['x'] + input['y']),
}

def eval_expr(expr : Tree):
    if (expr.data == "create_fn"):
        return {"expr" : eval_create_fn(expr, [])}
    elif (expr.children[0].data == "apply_fn"):
        # here the name of function is known
        return {"expr" : eval_apply_fn(expr.children[0], [])}
    elif (expr.children[0].data == "apply_fn_multiple"):
        return {"expr" : eval_apply_fn_multiple(expr.children[0], [])}

def eval_basic(basic : Tree):
    if (basic.data == "basic_int"):
        return {"basic_int" : int(basic.children[0].value)}
    elif (basic.data == "basic_word"):
        return {"basic_word" : basic.children[0].value}
    elif (basic.data == "create_prec_expr"):
        return {"create_prec_expr" : eval_expr(basic.children[0])}
    elif (basic.data == "create_pairs"):
        return {"create_pairs" : eval_pairs(basic.children[0])}

def eval_pairs(pairs : Tree, pairs_dict):
    if (pairs.data == "create_pair"):
        return {"create_pair" : eval_create_pair(pairs.children[0])}
    elif (pairs.data == "create_final_pair"):
        return {"create_pairs" : eval_final_pair(pairs.children[0])}

def eval_final_pair(pair : Tree):
    fns[pair.children[1].value] = eval_expr(pair.children[2])
    return {"create_final_pair" : f"{pair.children[1].value} = {eval_expr(pair.children[2])}"}
def eval_create_pair(pair : Tree):
    fns[pair.children[1].value] = eval_expr(pair.children[2])
    return [{"create_pair" : f"{pair.children[1].value} = {eval_expr(pair.children[2])}"},
            {"create_pairs": eval_pairs(pair.children[0])}]

def eval_apply_fn_multiple(apply_fn_multiple : Tree, args : [int]):

    inner = apply_fn_multiple.children[0].data

    parameter = eval_basic(apply_fn_multiple.children[1])

    if (inner == "apply_fn_multiple"):
        return {"apply_fn_multiple" : eval_apply_fn_multiple(apply_fn_multiple.children[0], args + [parameter])}
    elif (inner == "apply_fn"):
        return {"apply_fn_multiple" : eval_apply_fn(apply_fn_multiple.children[0], args + [parameter])}

def eval_apply_fn(apply_fn : Tree, args : [int]):
    if apply_fn.children[0].data == "basic_word":
        # find fn in dict
        res = None
        args.reverse()
        # check if all args are basic_int
        completely_evaluate = np.all([(len(a) == 1) & ("basic_int" in a) for a in args])
        for arg in args:
           res = fns[apply_fn.children[0].children[0].value](arg)
        return {"apply_fn" : res, "finihed" : ()}
    if apply_fn.children[0].data == "create_prec_expr":
        # anonymous function
        fn = eval_create_prec_expr(apply_fn.children[0])
        res = None
        args.reverse()
        for arg in args:
            res = fn # TODO apply fn to arg
        return {"apply_fn": res}

def eval_create_prec_expr(create_prec_expr : Tree):
    return {"create_prec_expr" : eval_expr(create_prec_expr.children[0])}

# probably better to move bound vars to eval_expr
def eval_create_fn(create_fn : Tree, bound_vars: [str]):
    var = create_fn.children[0].value
    if create_fn.children[1].data == 'create_fn':
        return {"create_fn" : eval_create_fn(create_fn.children[1], bound_vars + [var])}
    else:
        return {"create_fn" : eval_expr(create_fn.children[1])}

def eval_start(start : Tree):
    return { "start" : eval_expr(start.children[0])}



    def create_pair(self, items):
        return items[0] + items[2]

    def evaluate(self, fn, *args):
        if callable(fn):
            return fn(*args)
        else:
            return fn

# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    # read text file
    with open('test.txt', 'r') as file:
        data = file.read()
        l = Lark('''
                    start: expr
                    
                    expr: apply 
                        | WORD " -> " expr  -> create_fn
                        | WORD "->" expr -> create_fn  
                    
                    apply: basic   -> apply_fn
                        | apply " " basic    -> apply_fn_multiple
                    
                    basic: INT -> basic_int
                        | WORD   -> basic_word
                        |  "(" expr ")"  -> create_prec_expr
                        | "{ " pairs " }" -> create_pairs 
                        | "{" pairs "}" -> create_pairs 
                                        
                    pairs: WORD " = " expr   -> create_final_pair
                         | pairs ", " WORD " = " expr  -> create_pair

                    %import common.WORD   // imports from terminal library
                    %import common.INT    // imports from terminal library
                    // %ignore " "           // Disregard spaces in text
                 ''', parser='lalr')#, transformer=FunctionalTransformer())

        print(l.parse(data).pretty("-"))
        res = l.parse(data)
        r = eval_start(res)
        print(res)

