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





# Parse -> transform into better structure -> evaluate


"""
    data is the input string ~ well formatted code
"""
def get_output(data: str):
    l = Lark('''
                        start: expr
                            | multi_pairs " " expr -> create_env

                        expr: apply 
                            | WORD " -> " expr  -> create_fn
                            | WORD "->" expr -> create_fn  

                        apply: basic   -> apply_basic
                            | apply " " basic    -> apply_fn_multiple

                        basic: INT -> basic_int
                            | WORD   -> basic_word
                            |  "(" expr ")"  -> create_prec_expr
                            | multi_pairs -> create_pairs
                            
                        multi_pairs: "{ " pairs " }" -> create_pairs 
                            | "{" pairs "}" -> create_pairs 

                        pairs: WORD " = " expr   -> create_final_pair
                             | WORD "=" expr   -> create_final_pair
                             | pairs ", " WORD "=" expr  -> create_pair
                             | pairs ", " WORD " = " expr  -> create_pair

                        %import common.WORD   // imports from terminal library
                        %import common.INT    // imports from terminal library
                        // %ignore " "           // Disregard spaces in text
                     ''', parser='lalr')  # , transformer=FunctionalTransformer())

    # print(l.parse(data).pretty("-"))
    res = l.parse(data)
    r = eval_start(res)
    # print(r)
    # print("Evaluating with evaluator:")
    # print(to_str(r['env']))
    if r['env'] != {}:
        env_str = to_str(r['env'])
    evaluated = eval_expression(r, {})
    # print(evaluated)
    if r['env'] != {}:
        return (env_str + ' ' + to_str(evaluated))
    else:
        return to_str(evaluated)


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    # read text file
    with open('test.txt', 'r') as file:
        data = file.read()
        print(get_output(data))

        # test = Function(name="sqr", bound_vars={'x':None},inner_fn=functions['mult'], var_mapping={'x':'x', 'y':'x'})
        # print("Evaluating test:")
        # print(test.evaluate({'x' : Expression(is_fully_evaluated=True, value=2)}))

