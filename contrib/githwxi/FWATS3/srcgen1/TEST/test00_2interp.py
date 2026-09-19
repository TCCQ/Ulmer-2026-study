import unittest

from FWATS3_2basics import (
    D2Eapp, D2Efix, D2Eif0, D2Eint, D2Elam, D2Eop2, D2Estr, D2Evar, S2E000,
)
from FWATS3_2interp import D2Vint, D2Vstr, ENVcns, ENVnil, d2exp_evaluate


class InterpreterTests(unittest.TestCase):
    def test_string_literals_preserve_contents(self) -> None:
        for value in ("", "hello", "λ\n世界"):
            with self.subTest(value=value):
                self.assertEqual(d2exp_evaluate(D2Estr(value)), D2Vstr(value))

    def test_string_argument_passes_through_lambda(self) -> None:
        identity = D2Elam("x", S2E000(), D2Evar("x"))
        self.assertEqual(
            d2exp_evaluate(D2Eapp(identity, D2Estr("hello"))), D2Vstr("hello"),
        )

    def test_lambda_body_uses_captured_environment(self) -> None:
        annotation = S2E000()
        closure = d2exp_evaluate(
            D2Elam("x", annotation, D2Eop2("+", D2Evar("x"), D2Evar("y"))),
            ENVcns("y", D2Vint(10), ENVnil()),
        )
        caller = ENVcns("f", closure, ENVcns("y", D2Vint(100), ENVnil()))
        self.assertEqual(
            d2exp_evaluate(D2Eapp(D2Evar("f"), D2Eint(7)), caller),
            D2Vint(17),
        )

    def test_fix_body_can_call_itself(self) -> None:
        annotation = S2E000()
        factorial = D2Efix(
            "fact", "n", annotation,
            D2Eif0(
                D2Eop2("==", D2Evar("n"), D2Eint(0)),
                D2Eint(1),
                D2Eop2(
                    "*", D2Evar("n"),
                    D2Eapp(
                        D2Evar("fact"),
                        D2Eop2("-", D2Evar("n"), D2Eint(1)),
                    ),
                ),
            ),
            annotation,
        )
        self.assertEqual(
            d2exp_evaluate(D2Eapp(factorial, D2Eint(5))), D2Vint(120),
        )


if __name__ == "__main__":
    unittest.main()
