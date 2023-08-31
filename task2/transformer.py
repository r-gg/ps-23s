from lark import Tree
from main import *
import copy

def eval_expr(expr : Tree):
    if (expr.data == "create_fn"):
        return {"expr" : eval_create_fn(expr, [])}
    elif (expr.children[0].data == "apply_fn"):
        # here the name of function is known
        # return {"expr" : eval_apply_fn(expr.children[0], [])} # TODO check.
        pass
    elif (expr.children[0].data == "apply_fn_multiple"):
        return {"expr" : application_builder(expr.children[0])}

def eval_basic(basic : Tree) -> Expression:
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


# Handles arguments passed to a function and builds a function (no evaluation)
def application_builder(apply_fn_multiple : Tree) -> Function:

    curr = apply_fn_multiple

    done = False
    args = []
    while not done:
        inner = curr.children[0]

        parameter : Expression = eval_basic(curr.children[1]) # Evaluate if possible

        if (inner.data == "apply_fn_multiple"): # this means there are more arguments to be applied

            args.append(parameter)
            curr = inner

        elif (inner.data == "apply_fn"): # means this ends and the token in the child is either a predefined function name or an anonymous function

            args.append(parameter)


            args.reverse() # reversing params so that they are in good order (they are parsed inwards starting at the last argument)

            done = True # just in case

            if inner.children[0].data == "basic_word":
                # find fn in dict
                fn_name = inner.children[0].children[0].value
                if fn_name not in functions.keys():
                    raise "Unknown core function: " + fn_name


                inner_fn : CoreFunction = copy.copy(functions[fn_name]) # a copy of the core fn (using it as prototype) (it can have multiple instances)
                bound_vars = inner_fn.bound_vars
                for i, var_name in enumerate(bound_vars.keys()):
                    bound_vars[var_name] = args[i]

                new_fn_name = get_new_fn_name()
                new_fn = Function(name=new_fn_name,bound_vars = bound_vars,inner_fn=inner_fn)
                anon_fn_registry[new_fn_name] = new_fn
                return new_fn




            if inner.children[0].data == "create_prec_expr":
                # anonymous function
                if inner.children[0].children[0].data != 'create_fn':
                    raise "Invalid syntax: no create_fn in create_prec_expr where function was expected. It should be : '(x-> ...) 1', but something else was in '(...)'"

                # TODO: Change back
                inner_fn : Function =  copy.copy(functions['sqrt']) # eval_create_fn(inner.children[0].children[0]) # just calls the function creation (not application), application mode is below

                bound_vars = inner_fn.bound_vars
                for i, var_name in enumerate(bound_vars.keys()):
                    bound_vars[var_name] = args[i]

                new_fn_name = get_new_fn_name()
                new_fn = Function(name=new_fn_name, bound_vars=bound_vars, inner_fn=inner_fn)
                anon_fn_registry[new_fn_name] = new_fn
                return new_fn


# 3 modes:
# 1. creation (creates functions ~ skeletons)
# 2. application creation (applies arguments ~ sets bound variables)
# 3. evaluation (evaluates where possible)

# Functions can inherit bound vars e.g. (x->add ((y->mult x y) 2) 2) . The inner function uses a bound variable from the outer function
def eval_create_fn(create_fn : Tree, inherited_bound_vars:Dict[str, Expression] = {}) -> Function:

    # if there is one param that is an expression e.g. add (mult x x) y
    # here the first param is an expression, make sure to pass the bound variablles to it by mapping
    # not sure if the expression should be evaluated in create mode or application creation mode

    curr = create_fn

    vars = []

    done = False
    while not done:

        vars.append(curr.children[0].value)

        if curr.children[1].data == 'create_fn': # if there are more bound variables declared # uncurried x->y->....
            curr = curr.children[1]
        elif (curr.children[1].data == 'create_prec_expr') and (curr.children[1].children[0].data == 'create_fn'): # curried (x->(y-> ...)))
            curr = curr.children[1].children[0]
        else: # this is the final variable declared
            inner_expr = curr.children[0]
            fn : Function = create_mode_application_builder(inner_expr)
            done = True # just in case
            # TODO: Finish
            break



    return None


def create_mode_application_builder(application : Tree, new_bound_vars : List[str]) : # application is either apply_fn_multiple or apply_fn
    if application.data == 'apply_fn_multiple':

        # differentiate between expression and a basic_word
        # if it is a basic word and a bound variable make a mapping
        # if it is an expression, pass new bound vars to it and make it in create mode

        curr = application
        done = False

        args = []

        while not done:
            arg = curr.children[1]
            if arg.data == 'basic_word':
                name = arg.children[0].value
                # check if bound var or function name

                pass
            elif arg.data == 'basic_int':
                val = arg.children[0].value

                pass
            elif arg.data == 'create_prec_expr':
                pass

            if curr.children[ß].data == 'apply_fn':
                args.append(parameter)

                args.reverse()  # reversing params so that they are in good order (they are parsed inwards starting at the last argument)

                done = True  # just in case

                if inner.children[0].data == "basic_word":
                    # find fn in dict
                    fn_name = inner.children[0].children[0].value
                    if fn_name not in functions.keys():
                        raise "Unknown core function: " + fn_name

                    inner_fn: CoreFunction = copy.copy(functions[
                                                           fn_name])  # a copy of the core fn (using it as prototype) (it can have multiple instances)
                    bound_vars = inner_fn.bound_vars
                    for i, var_name in enumerate(bound_vars.keys()):
                        bound_vars[var_name] = args[i]

                    new_fn_name = get_new_fn_name()
                    new_fn = Function(name=new_fn_name, bound_vars=bound_vars, inner_fn=inner_fn)
                    anon_fn_registry[new_fn_name] = new_fn
                    return new_fn

                if inner.children[0].data == "create_prec_expr":
                    # anonymous function
                    if inner.children[0].children[0].data != 'create_fn':
                        raise "Invalid syntax: no create_fn in create_prec_expr where function was expected. It should be : '(x-> ...) 1', but something else was in '(...)'"

                    # TODO: Change back
                    inner_fn: Function = copy.copy(functions[
                                                       'sqrt'])  # eval_create_fn(inner.children[0].children[0]) # just calls the function creation (not application), application mode is below

                    bound_vars = inner_fn.bound_vars
                    for i, var_name in enumerate(bound_vars.keys()):
                        bound_vars[var_name] = args[i]

                    new_fn_name = get_new_fn_name()
                    new_fn = Function(name=new_fn_name, bound_vars=bound_vars, inner_fn=inner_fn)
                    anon_fn_registry[new_fn_name] = new_fn
                    return new_fn
        pass
    elif application.data == 'apply_fn':
        # either fn_name or prec_expr

        return copy.copy()
    pass




def eval_create_prec_expr(create_prec_expr : Tree):
    return {"create_prec_expr" : eval_expr(create_prec_expr.children[0])}

# initiates function creation and goes until all arguments are processed e.g. (x->y->z->....) or (x->(y->(z->...)))
# it gives a name to this function and saves it in the registry
def eval_create_fn(create_fn : Tree, bound_vars: [str]):
    var = create_fn.children[0].value
    if create_fn.children[1].data == 'create_fn':
        return {"create_fn" : eval_create_fn(create_fn.children[1], bound_vars + [var])}
    else:
        return {"create_fn" : eval_expr(create_fn.children[1])}

def eval_start(start : Tree):
    return { "start" : eval_expr(start.children[0])}

