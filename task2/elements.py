
from typing import Tuple

class Expression(object):
    def __init__(self):
        self.apply : Apply = None
        self.name_expr_pair : Tuple = None
        self.bound_vars : dict = {} # String -> Expression

    def __str__(self):
        return str(self.apply)

class Record(object):
    def __init__(self):
        self.name : str = None
        self.expr : Expression = None

class Apply(object):
    def __init__(self):
        self.basic : Basic = None
        self.apply : Apply = None



class Basic(object):

    def __init__(self):
        self.basic_int : int = None
        self.basic_word : str = None
        self.prec_expr : Expression = None
        self.pairs : [Pair] = None

class Pair(object):

    def __init__(self):
        self.name : str = None
        self.expr : Expression = None

class Function(object):

        def __init__(self):
            self.name : str = None
            self.bound_vars : dict = {} # String -> Expression
            self.inner_fn : Function = None # There is always one inner function that is called with the one bound parameter
            # e.g. f(x) = inner(x->inst)
            self.is_core_fn = False

        def can_evaluate(self):
            if len(self.bound_vars) != 0:
                return all([(p is not None) for p in self.bound_vars.values()])
            else:
                return True

        def set_vars(self, params : dict):
            for var in params.keys():
                if (var in self.bound_vars.keys()) and (params[var] is not None):
                    self.bound_vars[var] = params[var]

            if self.inner_fn is not None:
                self.inner_fn.set_vars(params)

        def evaluate(self, params : dict):
            self.set_vars(params)

            if (len(self.bound_vars) != 0) and self.can_evaluate():
                return self.inner_fn.evaluate(params)


class CoreFunction(Function):

    def __init__(self, name, lambda_fn):
        self.is_core_fn = True
        self.lambda_fn = None # lambda input: input['x'] + input['y']


    def evaluate(self, params : dict):
        self.set_vars(params)

        if (len(self.bound_vars) != 0) and self.can_evaluate():
            return self.lambda_fn(self.bound_vars) # ✅
        else:
            return self.lambda_fn()
