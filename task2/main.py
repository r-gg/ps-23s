# This is a sample Python script.
import copy

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

from lark import Lark, Transformer, v_args, Tree, Token

import numpy as np
from elements import *
from transformer import *
from evaluator import *
from printer import to_str
import uuid


def get_new_fn_name() -> str:
    return str(uuid.uuid4())

anon_fn_registry = {}

fns = {
        "mult" : lambda x, y: x * y,
        "div" : lambda x, y: x / y,
        "plus" : lambda x, y: x + y,
        "minus" : lambda x, y: x - y,
        # "cond" : lambda x, y, z: y if (x != 0) else z # TODO: cond logic should be implemented in evaluation, the only function that should be used is check cond
        "check_cond" : lambda x: x != 0
    }

functions = {
    "mult" : CoreFunction(name="mult", bound_vars={'x':None, 'y':None} , lambda_fn=lambda input: input['x'] * input['y']),
    "plus" : CoreFunction(name="plus",  bound_vars={'x':None, 'y':None} , lambda_fn=lambda input: input['x'] + input['y']),
    "sqrt" : CoreFunction(name="sqrt", bound_vars={'x':None} , lambda_fn=lambda input: input['x'] * input['x'])
}

# Environment for pairs
env : Dict[str,object] = {}


aliases_registry = {
    "a" : 3,
    "b" : Function("sqrt", bound_vars={'x': None},inner_fn=copy.copy(functions['mult']),var_mapping={'x':'x', 'y':'x'}) # (x->mult x x)
}


# Parse -> transform into better structure -> evaluate

# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    # read text file
    with open('test.txt', 'r') as file:
        data = file.read()
        l = Lark('''
                    start: expr
                        
                    
                    env: "{ " pairs " }" -> create_env 
                        | "{" pairs "}" -> create_env 
                    
                    expr: apply 
                        | WORD " -> " expr  -> create_fn
                        | WORD "->" expr -> create_fn  
                    
                    apply: basic   -> apply_basic
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
        print(r)
        print("Evaluating with evaluator:")
        evaluated = eval_expression(r, {})
        print(evaluated)
        print(to_str(evaluated))

        # test = Function(name="sqr", bound_vars={'x':None},inner_fn=functions['mult'], var_mapping={'x':'x', 'y':'x'})
        # print("Evaluating test:")
        # print(test.evaluate({'x' : Expression(is_fully_evaluated=True, value=2)}))

