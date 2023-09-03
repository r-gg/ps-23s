from lark import Tree
from typing import List, Dict
import global_vars
import copy




# Note: Bound vars are always passed as they can span multiple levels. In the evaluation all instances will be replaced when instantiated with actual values.

# Returns either fn_call or anonymous_fn or record or basic_int or basic_word or anything actually
def eval_expr(expr: Tree, bound_vars: List[str]):
    if (expr.data == "create_fn"):
        return eval_create_fn(expr, bound_vars)
    elif (expr.children[0].data == "apply_basic"):
        # here the name of function is known
        # return {"expr" : eval_apply_fn(expr.children[0], [])} # TODO check.
        return eval_basic(expr.children[0].children[0], bound_vars)
    elif (expr.data == "create_prec_expr"):
        return eval_expr(expr.children[0], bound_vars)
    elif (expr.children[0].data == "apply_fn_multiple"):
        return application_builder(expr.children[0], bound_vars)


def eval_basic(basic: Tree, bound_vars: List[str]) -> Dict:
    if (basic.data == "basic_int"):
        return {"type": "basic_int", "value": int(basic.children[0].value)}
    elif (basic.data == "basic_word"):
        val = basic.children[0].value
        if val in global_vars.fns.keys() or val == 'cond':
            # its a core function
            return {"type": "core_fn", "name": val}

        if val in bound_vars:
            return {"type": "basic_word_var", "value": val}

        if val in global_vars.environment.keys():
            return global_vars.environment[val]  # Immediately replacing aliases

        return {"type": "unknown_basic",
                "value": basic.children[0].value}
    elif (basic.data == "create_prec_expr"):
        return eval_expr(basic.children[0], bound_vars)
    elif (basic.data == "create_pairs"):
        return eval_create_pairs(basic.children[0], bound_vars)

    raise Exception("Something came in basic which shouldnt be here: " + basic.data)


"""
Returns {   "type": "record", 
            "pairs" : [ 
                    {"type" : "pair", 
                     "name": "_______", 
                     "value": { ... } } 
                ]
        }
"""


def eval_create_pairs(pairs: Tree, bound_vars: List[str]):
    return {"type": "record", "pairs": eval_pairs(pairs.children[0], bound_vars)}


"""
    :returns a list of pairs. eg.
                [ 
                    {"type" : "pair", 
                     "name": "_______", 
                     "value": { ... } } 
                ]
"""


def eval_pairs(pairs: Tree, bound_vars: List[str]) -> List[Dict]:
    res = []
    done = False
    curr = pairs

    while not done:

        if (curr.data == "create_pair"):
            name = curr.children[1].value
            inner = eval_expr(curr.children[2], bound_vars)
            res.append({"type": "pair", "name": name, "value": inner})
            curr = curr.children[0]
        elif (curr.data == "create_final_pair"):
            name = curr.children[0].value
            inner = eval_expr(curr.children[1], bound_vars)
            res.append({"type": "pair", "name": name, "value": inner})

            done = True  # just in case
            break
        elif curr.data == 'blank_pair':
            break
        else:
            raise Exception("Something is in pairs that shouldn't be there")

    res.reverse()  # must be reversed
    return res


# Handles arguments passed to a function and builds a function (no evaluation)
def application_builder(apply_fn_multiple: Tree, bound_vars: List[str]):
    curr = apply_fn_multiple

    args = []
    function = None

    inner = curr.children[0]
    parameter = eval_basic(curr.children[1], bound_vars)  # Evaluate if possible

    if inner.data == "apply_fn_multiple":  # this means there are more arguments to be applied
        function = application_builder(inner, bound_vars)
    elif inner.data == "apply_basic":  # means this ends and the token in the child is either a predefined function name or an anonymous function
        function = eval_basic(inner.children[0], bound_vars)

    return {"type": "fn_call",
            "param": parameter,
            "function": function}


# 3 modes:
# 1. creation (creates functions ~ skeletons)
# 2. application creation (applies arguments ~ sets bound variables)
# 3. evaluation (evaluates where possible)

# Functions can inherit bound vars e.g. (x->add ((y->mult x y) 2) 2) . The inner function uses a bound variable from the outer function
"""
returns {   "type" : "anon_fn" , 
            "bound_vars" " ['x', 'y'],
            "inner": { ...* }}
            
            * = expr : fn_call | pairs | int | name | another anon function 
"""


def eval_create_fn(create_fn: Tree, bound_vars: List[str]):
    # if there is one param that is an expression e.g. add (mult x x) y
    # here the first param is an expression, make sure to pass the bound variablles to it by mapping
    # not sure if the expression should be evaluated in create mode or application creation mode

    curr = create_fn

    vars: List[str] = []

    done = False
    inner = None
    while not done:

        vars.append(curr.children[0].value)

        if curr.children[1].data == 'create_fn':  # if there are more bound variables declared # uncurried x->y->....
            curr = curr.children[1]
        elif (curr.children[1].data == 'create_prec_expr'):  # curried (x->(y-> ...)))

            curr = curr.children[1].children[0]

        else:  # this is the final variable declared
            inner_expr = curr.children[0]

            inner = eval_expr(inner_expr, bound_vars + vars)

            done = True  # just in case
            break

    return {"type": "anon_fn", "bound_vars": vars, "inner": inner}


# def eval_create_prec_expr(create_prec_expr: Tree, bound_vars: List[str]):
#     return {"create_prec_expr": eval_expr(create_prec_expr.children[0], bound_vars)}


# initiates function creation and goes until all arguments are processed e.g. (x->y->z->....) or (x->(y->(z->...)))
# it gives a name to this function and saves it in the registry
def eval_create_fn(create_fn: Tree, bound_vars: [str]):
    var = create_fn.children[0].value
    if create_fn.children[1].data == 'create_fn':
        return {"type": "anon_fn", "bound_var": var, "inherited_bound_vars": bound_vars,
                "inner": eval_create_fn(create_fn.children[1], bound_vars + [var])}
    else:
        return {"type": "anon_fn", "bound_var": var, "inherited_bound_vars": bound_vars,
                "inner": eval_expr(create_fn.children[1], bound_vars + [var])}


def eval_env(env: Tree):
    return eval_create_pairs(env, [])


def eval_start(start: Tree):
    if start.data == 'create_env':
        return {"type": "start", "env": eval_env(start.children[0]), "expr": eval_expr(start.children[1], [])}
    else:
        return {"type": "start", "env": {}, "expr": eval_expr(start.children[0], [])}
