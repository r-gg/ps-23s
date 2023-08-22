# This is a sample Python script.

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

from lark import Lark, Transformer, v_args, Tree, Token


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
        "mult" : lambda x, y: x * y,
        "div" : lambda x, y: x / y,
        "plus" : lambda x, y: x + y,
        "minus" : lambda x, y: x - y,
        "cond" : lambda x, y, z: y if x else z
    }

def eval_expr(expr : Tree):
    pass

def eval_app(rest: Tree, args: [str]):
    if (len(rest.children) == 1):
        return fns[rest.children[0]](*args)
    pass





def eval_start(start : Tree):
    return { "start" : eval_expr(start.children[0])}




# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    # read text file
    with open('test.txt', 'r') as file:
        data = file.read()
        l = Lark('''
                    start: expr
                    
                    expr: apply 
                        | app 
                    
                    app: WORD " -> " expr   -> create_fn
                        | app "->" basic      -> create_fn
                    
                    apply: basic   -> apply_fn
                        | apply " " basic    -> apply_fn_multiple
                    
                    basic: INT -> basic_int
                        | WORD   -> basic_word
                        | prec_expr 
                        | "{ " pairs " }" -> create_pairs 
                        | "{" pairs "}" -> create_pairs 
                    
                    prec_expr : "(" expr ")"  
                    
                    pairs: WORD " = " expr   -> create_pair
                         | pairs ", " WORD " = " expr  -> create_final_pair

                    %import common.WORD   // imports from terminal library
                    %import common.INT    // imports from terminal library
                    // %ignore " "           // Disregard spaces in text
                 ''', parser='lalr')#, transformer=EvaluateExpr())

        print(l.parse(data).pretty("-"))
        res = l.parse(data)
        print(res)
