import unittest

from main import *


class MyTestCase(unittest.TestCase):

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


        # a gets evaluated in eval_env to 1. it should be a = x->1
        self.assertEqual(
            '{a = (x->cond 2 1 x)} 1',
            get_output('{a = x->cond 2 1 x} a 2')
        )


if __name__ == '__main__':
    unittest.main()
