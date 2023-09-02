
from typing import Tuple, Dict, List


# Each expression is a function invocation, either with bound variables or evaluated parameters
class Expression(object):
    def __init__(self, bound_vars = {}, inner_fn = None, is_partially_evaluated=False, is_fully_evaluated=False, value=None):
        self.bound_vars : Dict[str, Expression] = bound_vars # String -> Expression
        self.inner_fn : Function = inner_fn
        self.is_partially_evaluated : bool = is_partially_evaluated
        self.is_fully_evaluated : bool = is_fully_evaluated
        self.value : int = value



    def __str__(self):
        return str(self.apply)

class Record(object):
    def __init__(self):
        self.name : str = None
        self.expr : Expression = None

class Apply(Expression):
    def __init__(self):
        self.basic : Basic = None
        self.apply : Apply = None



class Basic(Apply):

    def __init__(self):
        self.basic_int : int = None
        self.basic_word : str = None
        self.prec_expr : Expression = None
        self.pairs : [Pair] = None

class Pair(object):

    def __init__(self):
        self.name : str = None
        self.expr : Expression = None

""" 
Each function can either  be a core function or have an inner function. It has its own bound variables defined by "x->y->..." 
and a variable mapping (inner_fn_var -> this_fn_var) indicating which bound_variables of this function correspond to which variables of the inner function. 
e.g. it makes a difference if this function is (x->y->minus x y). Here the mapping would be x->x and y->y. On the other hand
if (x->y->minus y x) is this function, var mapping would be x->y, y->x. 
TODO: Maybe when evaluating just pass a list of params. 

"""
class Function(object):

        def __init__(self, name=None, bound_vars={}, inner_fn = None, var_mapping = {}):
            self.name : str = name
            self.bound_vars : Dict[str, Expression] = bound_vars# String -> Expression
            self.inner_fn : Function = inner_fn # There is always one inner function that is called with the one bound parameter
            self.var_mapping : Dict[str, str] = var_mapping # variables of inner function -> variables of this function e.g. sqr x = mult x x meaning x -> x, y -> x if mult is inner function
            # e.g. f(x) = inner(x->inst)
            self.is_core_fn = False

        def can_evaluate(self):
            if len(self.bound_vars) != 0:
                return all([(p.is_fully_evaluated) for p in self.bound_vars.values()])
            else:
                return True

        def set_vars(self, params : Dict[str, Expression]):
            for var in params.keys():
                if (var in self.bound_vars.keys()) and (params[var] is not None):
                    self.bound_vars[var] = params[var]


        def map_vars_to_inner_fn(self) -> Dict[str, Expression]:
            res = {}
            for inner_fn_var_name, this_fn_var_name in self.var_mapping.items():
                if self.bound_vars[this_fn_var_name] is not None:
                    res[inner_fn_var_name] = self.bound_vars[this_fn_var_name]
            return res

        # evaluation mode is triggered recursively
        def evaluate(self, params : Dict[str, Expression]):
            self.set_vars(params)

            mapped_params = self.map_vars_to_inner_fn()


            # TODO: Evaluate each param


            if (len(self.bound_vars) != 0) and self.can_evaluate():
                return self.inner_fn.evaluate(mapped_params)

        def get_bound_vars(self):
            return self.bound_vars

        def set_bound_vars(self, bound_vars):
            self.bound_vars = bound_vars

class CoreFunction(Function):

    def __init__(self, name=None, bound_vars={}, lambda_fn = None):
        self.name: str = name
        self.bound_vars: Dict[str,Expression] = bound_vars  # String -> Expression

        self.is_core_fn = True

        self.lambda_fn = lambda_fn # lambda input: input['x'] + input['y']


    def extract_int_vals_from_bound_var_Expressions(self) -> Dict[str, int]:
        res = {}
        for bound_var, bound_expr in self.bound_vars.items():
            res[bound_var] = bound_expr.value
        return res

    def evaluate(self, params : Dict[str, Expression]):
        self.set_vars(params)

        if (len(self.bound_vars) != 0) and self.can_evaluate():
            extracted_dict = self.extract_int_vals_from_bound_var_Expressions()
            return self.lambda_fn(extracted_dict) # ✅
        else:
            return self.lambda_fn()
