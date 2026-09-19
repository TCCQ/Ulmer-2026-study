import unittest

from FWATS3_2basics import (
    D2C000, D2Cbind, D2Cimpl, D2Clocal, d2ecl, fnlist_cons, fnlist_nil,
    D2Etupl, D2Elets, d2exp, fnoptn_cons, fnoptn_nil,
    D2Eapp, D2Efix, D2Eif0, D2Eint, D2Elam, D2Eop2, D2Estr, D2Evar, S2E000,
)
from FWATS3_2interp import (
    D2Vtupl, d2val, D2Vnil, D2Vint, D2Vstr, ENVcns, ENVnil, d2exp_evaluate,
    d2ecl_evaluate, d2eclist_evaluate, d2env_search,
)


class InterpreterTests(unittest.TestCase):
    def test_empty_tuple(self) -> None:
        self.assertEqual(D2Vnil(), D2Vtupl(fnlist_nil()))
        self.assertEqual(
            d2exp_evaluate(D2Etupl(fnlist_nil())), D2Vtupl(fnlist_nil()),
        )

    def test_tuple_evaluates_elements_and_preserves_nesting(self) -> None:
        expression = D2Etupl(fnlist_cons[d2exp](
            D2Eop2("+", D2Evar("x"), D2Eint(1)),
            fnlist_cons[d2exp](
                D2Etupl(fnlist_cons[d2exp](D2Estr("nested"), fnlist_nil())),
                fnlist_nil(),
            ),
        ))
        expected = D2Vtupl(fnlist_cons[d2val](
            D2Vint(11),
            fnlist_cons[d2val](
                D2Vtupl(fnlist_cons[d2val](D2Vstr("nested"), fnlist_nil())),
                fnlist_nil(),
            ),
        ))
        self.assertEqual(
            d2exp_evaluate(expression, ENVcns("x", D2Vint(10), ENVnil())),
            expected,
        )

    def test_let_bindings_are_sequential_and_scoped(self) -> None:
        original = ENVcns("x", D2Vint(10), ENVnil())
        declarations = fnlist_cons[d2ecl](
            D2Cbind("x", D2Eop2("+", D2Evar("x"), D2Eint(1))),
            fnlist_cons[d2ecl](D2Cbind("y", D2Evar("x")), fnlist_nil()),
        )
        expression = D2Elets(declarations, fnoptn_cons[d2exp](D2Evar("y")))
        self.assertEqual(d2exp_evaluate(expression, original), D2Vint(11))
        self.assertEqual(d2env_search(original, "x"), D2Vint(10))

    def test_let_returned_closure_retains_bindings(self) -> None:
        expression = D2Elets(
            fnlist_cons[d2ecl](D2Cbind("x", D2Estr("captured")), fnlist_nil()),
            fnoptn_cons[d2exp](D2Elam("unused", S2E000(), D2Evar("x"))),
        )
        self.assertEqual(
            d2exp_evaluate(D2Eapp(expression, D2Eint(0))), D2Vstr("captured"),
        )

    def test_let_without_body_evaluates_declarations(self) -> None:
        self.assertEqual(
            d2exp_evaluate(D2Elets(fnlist_nil(), fnoptn_nil())),
            D2Vtupl(fnlist_nil()),
        )
        with self.assertRaises(TypeError):
            d2exp_evaluate(D2Elets(
                fnlist_cons[d2ecl](D2C000(), fnlist_nil()), fnoptn_nil(),
            ))

    def test_binding_shadows_after_evaluating_rhs(self) -> None:
        original = ENVcns("x", D2Vint(10), ENVnil())
        result = d2ecl_evaluate(
            D2Cbind("x", D2Eop2("+", D2Evar("x"), D2Eint(1))), original,
        )
        self.assertEqual(d2env_search(result, "x"), D2Vint(11))
        self.assertEqual(d2env_search(original, "x"), D2Vint(10))
        self.assertIsInstance(result, ENVcns)
        assert isinstance(result, ENVcns)
        self.assertIs(result.arg3, original)

    def test_local_exports_preserve_order_and_private_closures(self) -> None:
        original = ENVcns("hidden", D2Vint(100), ENVnil())
        private = fnlist_cons[d2ecl](
            D2Cbind("hidden", D2Eint(10)), fnlist_nil[d2ecl](),
        )
        public = fnlist_cons[d2ecl](
            D2Cbind("f", D2Elam("x", S2E000(), D2Evar("hidden"))),
            fnlist_cons[d2ecl](
                D2Cbind("y", D2Evar("hidden")),
                fnlist_cons[d2ecl](
                    D2Cbind("y", D2Eop2("+", D2Evar("y"), D2Eint(1))),
                    fnlist_nil[d2ecl](),
                ),
            ),
        )
        result = d2ecl_evaluate(D2Clocal(private, public), original)
        self.assertEqual(d2env_search(result, "hidden"), D2Vint(100))
        self.assertEqual(d2env_search(result, "y"), D2Vint(11))
        self.assertEqual(
            d2exp_evaluate(D2Eapp(D2Evar("f"), D2Eint(0)), result), D2Vint(10),
        )
        self.assertIs(
            d2ecl_evaluate(D2Clocal(private, fnlist_nil[d2ecl]()), original),
            original,
        )
        self.assertIs(d2eclist_evaluate(fnlist_nil[d2ecl](), original), original)

    def test_unsupported_declarations_raise(self) -> None:
        declarations: list[d2ecl] = [
            D2C000(), D2Cimpl("f", D2Eint(1), fnlist_nil(), fnlist_nil()),
        ]
        for declaration in declarations:
            with self.subTest(declaration=declaration):
                with self.assertRaises(TypeError):
                    d2ecl_evaluate(declaration, ENVnil())

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
