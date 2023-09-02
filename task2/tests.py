import unittest

from main import *

class MyTestCase(unittest.TestCase):
    def test_simple_configs(self):

        self.assertEqual( get_output(
            '(x->y->plus x 2) 1'),  # Input
            '3')   # Output

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

        self.assertEqual(get_output(
            '{a = x->plus x x, b = a 2} b'),  # Input
            '{a = (x->plus x x), b = a 2} 4')  # Output

        # self.assertEqual(get_output(
        #     '{a = x->plus x x, b = a 2}'),  # Input
        #     '{a = (x->plus x x), b = a 2}')  # Output # This is recognised as just a record in the main expression, not as environment!




if __name__ == '__main__':
    unittest.main()
