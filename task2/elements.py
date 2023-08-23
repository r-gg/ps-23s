
from typing import Tuple

class Expression(object):
    def __init__(self):
        self.apply : Apply = None
        self.name_expr_pair : Tuple = None

    def __str__(self):
        return str(self.apply)


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

