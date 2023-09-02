from global_vars import *
from typing import Dict, List


# Sets global variables/aliases
def eval_env(env: Dict):
    pairs = env['pairs']
    for pair in pairs:
        environment[pair['name']] = eval_expression(pair['value'] , {})


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
        if var_name in set_vars_in_context.keys():
            res = eval_expression(set_vars_in_context[var_name], set_vars_in_context)
            return res
        else:
            return input
    elif type == 'unknown_basic':
        if input['value'] in environment.keys():
            return eval_expression(environment[input['value']], set_vars_in_context)  # replace from the environment
        else:
            raise Exception(f"Unknown basic: {input}")


    elif type == 'core_fn':  # This is a core function without params
        fn_name = input['name']
        return fns[fn_name]()

    elif type == 'anon_fn':
        # Higher order functions # e.g function as a parameter
        inner = input['inner']
        inner_type = inner['type']
        if (inner_type == 'core_fn') and ('evaluation_status' in inner.keys()) and (inner['evaluation_status'] == 'partial'):
            params : List[Dict] = inner['params']
            new_param_was_set = False
            res_params = []
            for p in params:
                # if there was a new param set
                if p['type'] == 'basic_word_var':
                    if p['value'] in set_vars_in_context:
                        new_param_was_set = True
                        p = set_vars_in_context[p['value']]
                        res_params.append(p)
                else:
                    res_params.append(p)
            params = res_params

            # check if it can be evaluated only if new param was set
            if new_param_was_set and all([p['type'] == 'basic_int' for p in params]):  # Can be evaluated
                fn_name = inner['name']
                args = [p['value'] for p in params]
                result_int = fns[fn_name](*args)
                return {"type": "basic_int", "value": result_int}
            if new_param_was_set:
                inner['params'] = params # update inner params

            return input
        else:
            evaluated_inner = eval_expression(inner, set_vars_in_context)
            if evaluated_inner['type'] == 'basic_int':
                return evaluated_inner # fully evaluated
            else:
                # TODO: Check. Because checking if its partially evaluated is performed above
                input['inner'] = evaluated_inner # partially evaluated
                return input

    elif type == 'fn_call':
        # Transform the tree by replacing bound_var occurences with EVALUATED params if the function is anon_fn
        # otherwise (if its a core_fn) evaluate params and if they are ints evaluate core fn

        function = input['function']
        inner_fn_type = function['type']
        param = eval_expression(input['param'], set_vars_in_context)


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
            set_vars_in_context[bound_var] = param
            res = eval_expression(function, set_vars_in_context)
            if (res['type'] == 'anon_fn') and (res['bound_var'] == bound_var):
                return res['inner']
            else:
                return res

        elif inner_fn_type == 'unknown_basic':
            if function['value'] in environment.keys():
                input['function'] = environment[function['value']] # replace from the environment
                return eval_expression(input, set_vars_in_context)
            else:
                raise Exception(f"Unknown basic: {function}")

        elif inner_fn_type == 'core_fn':
            fn_name = function['name']
            if 'params' not in function.keys(): # closest param to the function
                function['params'] = [param]
                return function
            elif (len(function['params']) == 1) and (fn_name != 'cond'):
                params = function['params'] + [param]
                # otherwise (if its a core_fn) evaluate params and if they are ints evaluate core fn

                if all([p['type'] == 'basic_int' for p in params]):  # Can be evaluated
                    args = [p['value'] for p in params]
                    result_int = fns[fn_name](*args)
                    return {"type": "basic_int", "value": result_int}
                else:
                    function['params'] = params
                    function['evaluation_status'] = 'partial'
                    return function

            elif (len(function['params']) == 1) and (fn_name == 'cond'): # IF IT IS THE COND
                # Build further
                # return fncall with another param added if params length is 1
                function['params'].append(param)
                return function
            else:
                params = function['params'] + [param]
                assert (len(params) == 3) and (fn_name == 'cond')
                # TODO: Implement

                # otherwise (if its a core_fn) evaluate params and if they are ints evaluate core fn
                # TODO: Later add partial evaluation of 'cond' function (if condition is satisfied/not satisfied that one does not require the unused param to be evaluated
                #       the condition must in any case be evaluated
                pass

        pass