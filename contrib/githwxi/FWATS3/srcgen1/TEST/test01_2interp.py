"""Factorial and tuple-state Fibonacci in FWATS3 level-2 syntax."""

import math
import unittest

from FWATS3_2basics import (
    D2Eapp, D2Efix, D2Eif0, D2Eint, D2Elam, D2Eop2, D2Evar, S2E000,
    D2Eproj, D2Etupl, d2exp, fnlist_cons, fnlist_nil,
    D2Cbind, D2Elets, d2ecl, fnoptn_cons,
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


def tuple3(first: d2exp, second: d2exp, third: d2exp) -> D2Etupl:
    return D2Etupl(fnlist_cons[d2exp](
        first, fnlist_cons[d2exp](second, fnlist_cons[d2exp](third, fnlist_nil())),
    ))


def fibonacci_loop() -> D2Efix:
    """Build loop((n, a, b)) = if n == 0 then a else loop((n-1, b, a+b))."""
    state = D2Evar("state")
    n, a, b = D2Eproj(0, state), D2Eproj(1, state), D2Eproj(2, state)
    annotation = S2E000()
    return D2Efix(
        "fibo", "state", annotation,
        D2Eif0(
            D2Eop2("==", n, D2Eint(0)), a,
            D2Eapp(
                D2Evar("fibo"),
                tuple3(D2Eop2("-", n, D2Eint(1)), b, D2Eop2("+", a, b)),
            ),
        ),
        annotation,
    )


class TupleStateFibonacciTests(unittest.TestCase):
    def test_fibonacci_with_let_bindings(self) -> None:
        # let n = 10; fib = fibonacci_loop(); initial = (n, 0, 1)
        # in fib(initial) end
        declarations = fnlist_cons[d2ecl](
            D2Cbind("n", D2Eint(10)),
            fnlist_cons[d2ecl](
                D2Cbind("fib", fibonacci_loop()),
                fnlist_cons[d2ecl](
                    D2Cbind("initial", tuple3(D2Evar("n"), D2Eint(0), D2Eint(1))),
                    fnlist_nil(),
                ),
            ),
        )
        expression = D2Elets(
            declarations,
            fnoptn_cons[d2exp](D2Eapp(D2Evar("fib"), D2Evar("initial"))),
        )
        self.assertEqual(d2exp_evaluate(expression), D2Vint(55))

    def test_fibonacci(self) -> None:
        for n, expected in ((0, 0), (1, 1), (2, 1), (3, 2), (10, 55), (20, 6765)):
            with self.subTest(n=n):
                expression = D2Eapp(
                    fibonacci_loop(), tuple3(D2Eint(n), D2Eint(0), D2Eint(1)),
                )
                self.assertEqual(d2exp_evaluate(expression), D2Vint(expected))


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
