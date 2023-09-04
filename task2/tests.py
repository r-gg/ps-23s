import unittest

from main import *


class MyTestCase(unittest.TestCase):

    def test_currying(self):
        self.assertEqual('(x->(y->plus x y))',
                         get_output('x->(y->plus x y)'))

    def test_simple_configs(self):
        self.assertEqual(get_output(
            '(x->y->plus x 2) 1'),  # Input
            '3')  # Output

        self.assertEqual(get_output(
            '(x->y->plus x y) 1'),  # Input
            '(y->plus 1 y)')  # Output

        self.assertEqual(get_output(
            '(x->y->plus (mult x x) y) 1 2'),  # Input
            '3')  # Output

        self.assertEqual(get_output(
            '(x->y->plus (mult x x) y) 2 2'),  # Input
            '6')  # Output

        self.assertEqual(get_output(
            '(x->(y->plus (mult x x) y)) 2 2'),  # Input
            '6')  # Output

        self.assertEqual(get_output(
            '(x->(y->plus (mult x x) y)) (2) (2)'),  # Input
            '6')  # Output

        self.assertEqual(get_output(
            '(x->(y->plus (mult (x) x) y)) (2) (2)'),  # Input
            '6')  # Output

        self.assertEqual(get_output(
            '(x->y->plus (mult x x) y) 2'),  # Input
            '(y->plus 4 y)')  # Output

        self.assertEqual(get_output(
            'x->y->plus (mult x x) y'),  # Input
            '(x->(y->plus (mult x x) y))')  # Output

    def test_with_env(self):
        self.assertEqual(  # Input
            '{a = (x->plus x x), b = a 2} 4',
            get_output(
                '{a = x->plus x x, b = a 2} b')
        )  # Output

        # self.assertEqual(get_output(
        #     '{a = x->plus x x, b = a 2}'),  # Input
        #     '{a = (x->plus x x), b = a 2}')  # Output # This is recognised as just a record in the main expression, not as environment!

    def test_complex_env(self):
        self.assertEqual(get_output(
            'minus ((x -> y -> plus (mult x x) y) 2 5) ((x -> y -> plus (mult x x) y) 2 3)'),
            '2'
        )

        self.assertEqual(
            '{a = (x->(y->plus (mult x x) y)), b = a 2, c = b 3} 2',
            get_output(
                '{a=x->y->plus (mult x x) y, b=a 2, c=b 3} minus (b 5) c')
        )

    def test_cond_basic(self):
        self.assertEqual(
            '1',
            get_output('cond 1 1 2')
        )

        self.assertEqual(
            '2',
            get_output('cond 0 1 2')
        )

        self.assertEqual(
            '2',
            get_output('cond {} 1 2')
        )

        self.assertEqual(
            '1',
            get_output('cond {a = 3} 1 2')
        )


    def test_cond_simple(self):
        self.assertEqual(
            '{a = cond {} 1 2} 2',
            get_output('{a = cond {} 1 2} a')
        )

        self.assertEqual(
            '{a = (x->cond {} 1 x)} 2',
            get_output('{a = x->cond {} 1 x} a 2')
        )


        # a gets evaluated in eval_env to 1. so there is a function call "1 2" in the main -> just returns the 1 (marked as function), 2 is redundant
        self.assertEqual(
            '{a = (x->cond 2 1 x)} 1',
            get_output('{a = x->cond 2 1 x} a 2')
        )

    """ Records themselves can be evaluated and use aliases later in the record """
    def advanced_record_evaluation(self):
        self.assertEqual(
            '{a = 1, b = (x->plus x 1), c = 3}',
            get_output('{a = x->cond 2 1 x, b = x->plus x a, c = b 2}')
        )


    def test_list_example(self):
        self.assertEqual(
            '{list = (c->(f->(x->cond (c x) {val = x, nxt = list c f (f x)} {}))), reduce = (f->(x->(lst->cond lst (f (reduce f x (lst nxt)) (lst val)) x))), range = (a->(b->list ((x->minus b x)) ((x->plus 1 x)) a)), sum = (lst->reduce ((x->(y->plus x y))) 0 lst)} 12',
            get_output(
                '{list = c -> f -> x -> cond (c x) { val = x, nxt = list c f (f x) } {}, reduce = f -> x -> lst -> cond lst (f (reduce f x (lst nxt)) (lst val)) x, range = a -> b -> list (x -> minus b x) (x -> plus 1 x) a, sum = lst -> reduce (x -> y -> plus x y) 0 lst} sum (range 3 6)'
            )
        )

    def test_eval_records_as_functions(self):
        self.assertEqual(
            '5',
            get_output('plus ({a = 3} a) 2')
        )

        self.assertEqual(
            '{a = {c = 3}, b = plus (a c) 2} 5',
            get_output('{a = {c = 3}, b = plus (a c) 2} b')
        )

    def test_lists(self):

        self.assertEqual(
            '{list = (c->(f->(x->cond (c x) {val = x, nxt = list c f (f x)} {}))), range = (a->(b->list ((x->minus b x)) ((x->plus 1 x)) a))} {val = 1, nxt = {}}',
            get_output('{list = c -> f -> x -> cond (c x) { val = x, nxt = list c f (f x) } {}, range = a -> b -> list (x -> minus b x) (x -> plus 1 x) a} range 1 2')
        )


    def test_variable_scoping_and_higher_order_fns(self):
        self.assertEqual(
            '1',
            get_output('(x -> x) (x -> x) 1')
        )
        self.assertEqual(
            '1',
            get_output('(x -> (x -> x) 1) 2')
        )

    def test_create_list_with_range(self):
        self.assertEqual(
            '{list = (c->(f->(x->cond (c x) {val = x, nxt = list c f (f x)} {}))), range = (a->(b->list ((x->minus b x)) ((x->plus 1 x)) a))} {val = 1, nxt = {val = 2, nxt = {val = 3, nxt = {val = 4, nxt = {val = 5, nxt = {val = 6, nxt = {}}}}}}}',
            get_output('{list = c -> f -> x -> cond (c x) { val = x, nxt = list c f (f x) } {}, range = a -> b -> list (x -> minus b x) (x -> plus 1 x) a} range 1 7')
        )

if __name__ == '__main__':
    unittest.main()
