"""Accumulator-based factorial expressed in FWATS3 level-2 syntax."""

import math
import unittest

from FWATS3_2basics import (
    D2Eapp, D2Efix, D2Eif0, D2Eint, D2Elam, D2Eop2, D2Evar, S2E000,
)
from FWATS3_2interp import D2Vint, d2exp_evaluate


def factorial_loop() -> D2Efix:
    """Build loop(n)(acc) = if n == 0 then acc else loop(n-1)(n*acc).

    The recursive call is in tail position. The interpreter still uses the
    Python call stack; this example does not require tail-call optimization.
    """
    annotation = S2E000()
    return D2Efix(
        "loop", "n", annotation,
        D2Elam(
            "acc", annotation,
            D2Eif0(
                D2Eop2("==", D2Evar("n"), D2Eint(0)),
                D2Evar("acc"),
                D2Eapp(
                    D2Eapp(
                        D2Evar("loop"),
                        D2Eop2("-", D2Evar("n"), D2Eint(1)),
                    ),
                    D2Eop2("*", D2Evar("n"), D2Evar("acc")),
                ),
            ),
        ),
        annotation,
    )


class TailRecursiveFactorialTests(unittest.TestCase):
    def test_factorial(self) -> None:
        # factorial(n) = loop(n)(1), including the base case 0! = 1.
        for n in (0, 1, 2, 5, 10, 20):
            with self.subTest(n=n):
                expression = D2Eapp(
                    D2Eapp(factorial_loop(), D2Eint(n)), D2Eint(1),
                )
                self.assertEqual(d2exp_evaluate(expression), D2Vint(math.factorial(n)))

    def test_loop_preserves_initial_accumulator(self) -> None:
        for n in (0, 5):
            with self.subTest(n=n):
                expression = D2Eapp(
                    D2Eapp(factorial_loop(), D2Eint(n)), D2Eint(3),
                )
                self.assertEqual(
                    d2exp_evaluate(expression), D2Vint(3 * math.factorial(n)),
                )


if __name__ == "__main__":
    unittest.main()
