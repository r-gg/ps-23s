import copy

import global_vars
from typing import Dict, List

import uuid

def generate_random_hash():
    return str(uuid.uuid4()).replace('-', '')

from printer import to_str
# Sets global variables/aliases
def eval_env(env: Dict):
    pairs = env['pairs']
    for pair in pairs:
        global_vars.environment[pair['name']] = eval_expression(pair['value'] , {})


# renames the variable in the scope of the expr.
def rename_in_scope(expr: Dict, bound_var: str, new_name: str) -> Dict:
    type = expr['type']
    if type == 'basic_word_var':
        if expr['value'] == bound_var:
            expr['value'] = new_name
        return expr
    elif type == 'core_fn':
        return expr
    elif type == 'anon_fn':
        if expr['bound_var'] == bound_var:
            expr['bound_var'] = new_name
        expr['inner'] = rename_in_scope(expr['inner'], bound_var, new_name)
        return expr
    elif type == 'fn_call':
        expr['function'] = rename_in_scope(expr['function'], bound_var, new_name)
        expr['param'] = rename_in_scope(expr['param'], bound_var, new_name)
        return expr
    elif type == 'basic_int':
        return expr
    elif type == 'unknown_basic':
        return expr
    elif type == 'record':
        for pair in expr['pairs']:
            pair['value'] = rename_in_scope(pair['value'], bound_var, new_name)
        return expr

def check_for_bound_var_bindings(expr: Dict, bound_var : str) -> bool:
    type = expr['type']
    if type == 'basic_word_var':
        return False
    elif type == 'core_fn':
        return False
    elif type == 'anon_fn':
        if expr['bound_var'] == bound_var:
            return True
        else:
            return check_for_bound_var_bindings(expr['inner'], bound_var)
    elif type == 'fn_call':
        return check_for_bound_var_bindings(expr['function'], bound_var) or check_for_bound_var_bindings(expr['param'], bound_var)
    elif type == 'basic_int':
        return False
    elif type == 'unknown_basic':
        return False
    elif type == 'record':
        for pair in expr['pairs']:
            if check_for_bound_var_bindings(pair['value'], bound_var):
                return True
        return False
def find_val_of_innermost_var(var_name: str, set_vars_in_context: Dict[str, Dict]) -> Dict:
    if var_name in set_vars_in_context.keys():
        # all_vars_with_this_name = [v for v in set_vars_in_context.keys() if v.startswith(var_name)]
        # all_vars_with_this_name.sort()
        # innermost_var = all_vars_with_this_name[-1]
        # return set_vars_in_context[innermost_var]
        return set_vars_in_context[var_name]
    else:
        raise Exception(f"Variable {var_name} not found in context")


# Returns new_set_vars_in_context with the new var added
# TODO: Refactor back to previous state both functions, add and find_val_of_innermost_var
def add_new_var_to_context(var_name: str, var_value: Dict, new_set_vars_in_context: Dict[str, Dict]) -> Dict[str, Dict]:
    # if var_name in new_set_vars_in_context.keys():
    #     all_vars_with_this_name = [v for v in new_set_vars_in_context.keys() if v.startswith(var_name)]
    #     all_vars_with_this_name.sort()
    #     depth = len(all_vars_with_this_name)
    #     if depth == 1:
    #         new_name = var_name + '---1'
    #         new_set_vars_in_context[new_name] = var_value
    #     else:
    #         new_name = var_name + '---' + str(depth)
    #         new_set_vars_in_context[new_name] = var_value
    # else:
    #     new_set_vars_in_context[var_name] = var_value

    new_set_vars_in_context[var_name] = var_value
    return new_set_vars_in_context

# variables that are set in this context are passed to evaluation
def eval_expression(input: Dict, set_vars_in_context: Dict[str, Dict]) -> Dict:
    # Base cases
    type = input['type']
    if type == 'start':
        if input['env'] != {}:
            eval_env(input['env'])
        return eval_expression(input['expr'], {})
    elif type == 'basic_int':
        return input
    elif type == 'basic_word_var':
        var_name = input['value']
        if var_name in set_vars_in_context.keys() and input != find_val_of_innermost_var(var_name, set_vars_in_context):
            val = find_val_of_innermost_var(var_name, set_vars_in_context)
            res = eval_expression(val, set_vars_in_context)
            return res
        else:
            return input
    elif type == 'unknown_basic':
        if input['value'] in global_vars.environment.keys():
            return eval_expression(global_vars.environment[input['value']], set_vars_in_context)  # replace from the global_vars.environment
        elif input['value'] in set_vars_in_context.keys(): # needed for evaluating records
            val = find_val_of_innermost_var(input['value'], set_vars_in_context)
            return eval_expression(val, set_vars_in_context)
        else:
            raise Exception(f"Unknown basic: {input}")
    elif type == 'record':
        new_set_vars_in_context = copy.copy(set_vars_in_context)
        pairs = input['pairs']
        evaluated_pairs = []
        for pair in pairs:
            name = pair['name']
            val = pair['value']
            evaluated_pair = eval_expression(val, new_set_vars_in_context)
            new_set_vars_in_context = add_new_var_to_context(name, evaluated_pair, new_set_vars_in_context)
            evaluated_pairs.append({"type": "pair",
                                    "name": name,
                                    "value": evaluated_pair})
        input['pairs'] = evaluated_pairs
        return input

    elif type == 'core_fn':  # This is a core function without params
        fn_name = input['name']

        # TODO Add handling of cond function here somewhere


        if 'evaluation_status' in input.keys() and input['evaluation_status'] == 'partial':
            if fn_name == 'cond':
                params = input['params']
                assert len(params) == 3
                condition = eval_expression(params[0], set_vars_in_context)
                is_cond_evaluatable = condition['type'] == 'basic_int' or condition['type'] == 'record'
                if is_cond_evaluatable:
                    is_cond_true = (condition['type'] == 'basic_int' and condition['value'] != 0) or \
                                   (condition['type'] == 'record' and len(condition['pairs']) != 0)
                    if is_cond_true:
                        evaluated_param_true = eval_expression(params[1], set_vars_in_context)
                        return evaluated_param_true
                    else:
                        evaluated_param_false = eval_expression(params[2], set_vars_in_context)
                        return evaluated_param_false
                else:
                    return input


            params: List[Dict] = input['params']
            new_param_was_set = False
            res_params = []
            for p in params:
                if p['type'] != 'basic_word_var':
                    evaluated_param = eval_expression(p, set_vars_in_context)
                    p = evaluated_param
                # if there was a new param set
                if p['type'] == 'basic_word_var':
                    if p['value'] in set_vars_in_context:
                        val = find_val_of_innermost_var(p['value'] , set_vars_in_context)

                        new_param_was_set = True
                        p = val
                        res_params.append(p)
                else:
                    res_params.append(p)
            params = res_params

            # check if it can be evaluated only if new param was set
            if new_param_was_set and all([p['type'] == 'basic_int' for p in params]):  # Can be evaluated
                fn_name = input['name']
                args = [p['value'] for p in params]
                result_int = global_vars.fns[fn_name](*args)
                return {"type": "basic_int", "value": result_int}
            if new_param_was_set:
                input['params'] = params  # update inner params

            return input


        print("Core fn uncaught")

    elif type == 'anon_fn':
        # Higher order functions # e.g function as a parameter
        inner = input['inner']
        inner_type = inner['type']
        if (inner_type == 'core_fn') and ('evaluation_status' in inner.keys()) and (inner['evaluation_status'] == 'partial'):
            fn_name = inner['name']
            # TODO: continue here
            if fn_name == 'cond':
                params = inner['params']
                assert len(params) == 3
                condition = eval_expression(params[0], set_vars_in_context)
                is_cond_evaluatable = condition['type'] == 'basic_int' or condition['type'] == 'record'
                if is_cond_evaluatable:
                    is_cond_true = (condition['type'] == 'basic_int' and condition['value'] != 0) or \
                                   (condition['type'] == 'record' and len(condition['pairs']) != 0)
                    if is_cond_true:
                        evaluated_param_true = eval_expression(params[1], set_vars_in_context)
                        return evaluated_param_true
                    else:
                        evaluated_param_false = eval_expression(params[2], set_vars_in_context)
                        return evaluated_param_false
                else:
                    return input

            params : List[Dict] = inner['params']
            new_param_was_set = False
            res_params = []
            for p in params:
                if p['type'] != 'basic_word_var':
                    evaluated_param = eval_expression(p, set_vars_in_context)
                    p = evaluated_param
                # if there was a new param set
                if p['type'] == 'basic_word_var':
                    if p['value'] in set_vars_in_context:
                        val = find_val_of_innermost_var(p['value'], set_vars_in_context)
                        new_param_was_set = True
                        p = val

                res_params.append(p)
            params = res_params

            # check if it can be evaluated only if new param was set
            if new_param_was_set and all([p['type'] == 'basic_int' for p in params]):  # Can be evaluated
                fn_name = inner['name']
                args = [p['value'] for p in params]
                result_int = global_vars.fns[fn_name](*args)
                return {"type": "basic_int", "value": result_int}

            inner['params'] = params # update inner params

            return input
        else:
            evaluated_inner = eval_expression(inner, set_vars_in_context)

            if (evaluated_inner['type'] == 'basic_int' or evaluated_inner['type'] == 'record'): # and inner_type != 'cond' :
                return evaluated_inner # fully evaluated
            else:
                # if its cond then also return inner (e.g. if cond evaluates to 1 return x->1)
                # TODO: Check. Because checking if its partially evaluated is performed above
                input['inner'] = evaluated_inner # partially evaluated
                return input

    elif type == 'fn_call':
        # Transform the tree by replacing bound_var occurences with EVALUATED params if the function is anon_fn
        # otherwise (if its a core_fn) evaluate params and if they are ints evaluate core fn

        function = input['function']
        inner_fn_type = function['type']

        # param = eval_expression(input['param'], set_vars_in_context)

        if inner_fn_type == 'fn_call':
            # TODO: Evaluate the inner expr first and then set the parameter and then evaluate this fn call again
            # Keep evaluating until the inner fn type changes to anon_fn or core_fn
            input['function'] = eval_expression(function, set_vars_in_context)

            # Inner function can also return an int? TODO

            # if ('evaluation_status' in input['function'].keys()) and input['function']['evaluation_status'] == 'partial':
            #     return input['function']
            return eval_expression(input, set_vars_in_context)

        elif inner_fn_type == 'anon_fn':
            bound_var = function['bound_var']
            new_set_vars_in_context = copy.copy(set_vars_in_context)

            param = eval_expression(input['param'], set_vars_in_context)
            if check_for_bound_var_bindings(param, bound_var):
                param = rename_in_scope(param, bound_var, bound_var + '-' + generate_random_hash())
            new_set_vars_in_context = add_new_var_to_context(bound_var, param, new_set_vars_in_context)
            res = eval_expression(function, new_set_vars_in_context)
            if (res['type'] == 'anon_fn') and (res['bound_var'] == bound_var):
                return res['inner']
            else:
                return res

        elif inner_fn_type == 'unknown_basic':
            if function['value'] in global_vars.environment.keys():
                input['function'] = global_vars.environment[function['value']] # replace from the global_vars.environment
                return eval_expression(input, set_vars_in_context)
            elif function['value'] in set_vars_in_context.keys(): # needed for evaluating records
                val = find_val_of_innermost_var(function['value'], set_vars_in_context)
                input['function'] = val
                return eval_expression(input,set_vars_in_context)
            else:
                raise Exception(f"Unknown basic: {function}")

        elif inner_fn_type == 'basic_int': # function already evaluated (probably by a cond) so this arg is redundant
            # e.g. {a = x->cond 2 1 x} a 2 ----> evaluating a by setting x to 2 is redundant since a=1 already
            return function
        elif inner_fn_type == 'record': # could be selecting a field from the record i.e. {nxt = 1, val = 2} val  ====> 2
            param = input['param']
            if param['type'] == 'unknown_basic':
                if param['value'] in [ pair['name'] for pair in function['pairs'] ]:
                    return [ pair['value'] for pair in function['pairs'] if pair['name'] == param['value'] ][0]

        elif inner_fn_type == 'basic_word_var':
            var_name = function['value']
            if var_name in set_vars_in_context.keys():
                val = find_val_of_innermost_var(var_name, set_vars_in_context)
                res = eval_expression(val, set_vars_in_context)
                return res
            elif var_name in global_vars.environment.keys():
                res = eval_expression(global_vars.environment[var_name], set_vars_in_context)
                return res
            else:
                return input

        elif inner_fn_type == 'core_fn':
            fn_name = function['name']
            if 'params' not in function.keys(): # closest param to the function
                param = eval_expression(input['param'], set_vars_in_context) # first level of cond as well
                function['params'] = [param]
                return function
            elif (len(function['params']) == 1) and (fn_name != 'cond'):
                param = eval_expression(input['param'], set_vars_in_context)
                params = function['params'] + [param]
                # otherwise (if its a core_fn) evaluate params and if they are ints evaluate core fn

                if all([p['type'] == 'basic_int' for p in params]):  # Can be evaluated # they all need to be int since its not cond (-> it is an arithmetic operation)
                    args = [p['value'] for p in params]
                    result_int = global_vars.fns[fn_name](*args)
                    return {"type": "basic_int", "value": result_int}
                else:
                    function['params'] = params
                    function['evaluation_status'] = 'partial'
                    return function

            elif (len(function['params']) == 1) and (fn_name == 'cond'): # IF IT IS THE COND
                # Build further
                # return fncall with another param added if params length is 1
                # PARAM NOT EVALUATED YET
                # The param here is the one which gets returned if condition is True
                function['params'].append(input['param'])
                return function
            elif (len(function['params']) == 2) and fn_name == 'cond':
                params = function['params'] + [input['param']]
                assert (len(params) == 3) and (fn_name == 'cond')
                # TODO: Implement
                condition = params[0]
                is_cond_evaluatable = condition['type'] == 'basic_int' or condition['type'] == 'record'
                if is_cond_evaluatable:
                    is_cond_true = (condition['type'] == 'basic_int' and condition['value'] != 0) or \
                                   (condition['type'] == 'record' and len(condition['pairs']) != 0)
                    if is_cond_true:
                        evaluated_param_true = eval_expression(params[1], set_vars_in_context)
                        return evaluated_param_true
                    else:
                        evaluated_param_false = eval_expression(params[2], set_vars_in_context)
                        return evaluated_param_false
                else:
                    function['params'] = params
                    function['evaluation_status'] = 'partial'
                    return function
                # otherwise (if its a core_fn) evaluate params and if they are ints evaluate core fn
                # TODO: Later add partial evaluation of 'cond' function (if condition is satisfied/not satisfied that one does not require the unused param to be evaluated
                #       the condition must in any case be evaluated
            else:
                print("Core fn in fn call that satisfies nothing probably from the environment !")
        raise Exception("Something slipped")
        pass
