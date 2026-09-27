"""Programs built from templates: resolve them, then interpret the result.

A template body may refer to variables of the scope that surrounds the use,
and the body replaces the use in place. A program therefore binds those
variables with a let expression and leaves the template use as the body.
"""

import math
import unittest

from FWATS3_2basics import (
    D2Eapp, D2Ecst, D2Efix, D2Eif0, D2Eint, D2Elam, D2Elets, D2Eop1, D2Eop2,
    D2Eproj, D2Etapp, D2Etupl, D2Estr, D2Evar, D2Cbind, D2Cimpl,
    d2exp, d2explst, d2ecl, S2Econ, S2Efun, S2Evar, s2exp, s2explst, strn,
    fnlist_cons, fnlist_nil, fnoptn_cons,
)
from FWATS3_2interp import D2Vint, D2Vstr, D2Vtupl, d2val, d2exp_evaluate
from FWATS3_2staexp import CTXnil, s2ctx
from FWATS3_2template import s2tmp_collect, t2_replace


def s2lst(items: list[s2exp]) -> s2explst:
    result: s2explst = fnlist_nil()
    for item in reversed(items):
        result = fnlist_cons(item, result)
    return result


def d2lst(items: list[d2exp]) -> d2explst:
    result: d2explst = fnlist_nil()
    for item in reversed(items):
        result = fnlist_cons(item, result)
    return result


def con(name: strn) -> S2Econ:
    """Build a type constructor without arguments, such as Nat or Bool."""
    return S2Econ(name, fnlist_nil())


def arr(arg1: s2exp, arg2: s2exp) -> S2Econ:
    """Build Arr(arg1, arg2)."""
    return S2Econ("Arr", s2lst([arg1, arg2]))


def impl_ctx(*decls: d2ecl) -> s2ctx:
    """Collect implementations in order, newest declaration nearest the front."""
    ctx: s2ctx = CTXnil()
    for decl in decls:
        found = s2tmp_collect(decl, ctx)
        assert isinstance(found, fnoptn_cons)
        ctx = found.arg1
    return ctx


def resolved(dexp: d2exp, ctx: s2ctx) -> d2exp:
    """Replace every template use in the expression and return the result."""
    result = t2_replace(dexp, ctx)
    assert isinstance(result, fnoptn_cons)
    body: d2exp = result.arg1
    return body


def pair_impl() -> D2Cimpl:
    """Build the template pair@a,b := (x, y), which captures x and y."""
    return D2Cimpl("pair", D2Etupl(d2lst([D2Evar("x"), D2Evar("y")])),
                   fnlist_nil(), s2lst([S2Evar("a"), S2Evar("b")]))


def swap_impl() -> D2Cimpl:
    """Build the template swap@a := (p.1, p.0), which captures p."""
    return D2Cimpl(
        "swap",
        D2Etupl(d2lst([D2Eproj(1, D2Evar("p")), D2Eproj(0, D2Evar("p"))])),
        fnlist_cons[strn]("a", fnlist_nil()), s2lst([S2Evar("a")]),
    )


def twice_impl() -> D2Cimpl:
    """Build the template twice@(a -> a), b := f(f(x))."""
    a = S2Evar("a")
    return D2Cimpl(
        "twice", D2Eapp(D2Evar("f"), D2Eapp(D2Evar("f"), D2Evar("x"))),
        fnlist_cons[strn]("a", fnlist_cons[strn]("b", fnlist_nil())),
        s2lst([S2Efun(a, a), S2Evar("b")]),
    )


def fact_impl() -> D2Cimpl:
    """Build the template fact@a := fix f(n) . if n == 0 then 1 else n * f(n-1)."""
    a = S2Evar("a")
    return D2Cimpl(
        "fact",
        D2Efix(
            "f", "n", a,
            D2Eif0(
                D2Eop2("==", D2Evar("n"), D2Eint(0)),
                D2Eint(1),
                D2Eop2(
                    "*", D2Evar("n"),
                    D2Eapp(D2Evar("f"),
                           D2Eop2("-", D2Evar("n"), D2Eint(1))),
                ),
            ),
            S2Efun(a, a),
        ),
        fnlist_cons[strn]("a", fnlist_nil()), s2lst([a]),
    )


def inc_impl() -> D2Cimpl:
    """Build the template inc@Nat := n + 1, which captures n."""
    return D2Cimpl("inc", D2Eop1("+1", D2Evar("n")), fnlist_nil(),
                   s2lst([con("Nat")]))


def inc2_impl() -> D2Cimpl:
    """Build the template inc2@a := inc@a(n), which uses another template."""
    return D2Cimpl("inc2", D2Etapp(D2Ecst("inc"), s2lst([S2Evar("a")])),
                   fnlist_cons[strn]("a", fnlist_nil()), s2lst([S2Evar("a")]))


def inc3_impl() -> D2Cimpl:
    """Build the template inc3@a := inc@a + inc@a, with two nested uses."""
    a = S2Evar("a")
    use = D2Etapp(D2Ecst("inc"), s2lst([a]))
    return D2Cimpl("inc3", D2Eop2("+", use, use),
                   fnlist_cons[strn]("a", fnlist_nil()), s2lst([a]))


class PairAndSwapTests(unittest.TestCase):
    def test_pair_and_swap_evaluate_after_replacement(self) -> None:
        ctx = impl_ctx(pair_impl(), swap_impl())
        text = con("Str")
        # let x = 1; y = "hello"; p = pair@[Nat,Str]
        # in swap@[Arr(Nat,Str)] end
        program = D2Elets(
            fnlist_cons[d2ecl](
                D2Cbind("x", D2Eint(1)),
                fnlist_cons[d2ecl](
                    D2Cbind("y", D2Estr("hello")),
                    fnlist_cons[d2ecl](
                        D2Cbind("p", D2Etapp(D2Ecst("pair"),
                                            s2lst([con("Nat"), text]))),
                        fnlist_nil[d2ecl](),
                    ),
                ),
            ),
            fnoptn_cons[d2exp](
                D2Etapp(D2Ecst("swap"), s2lst([arr(con("Nat"), text)])),
            ),
        )
        self.assertEqual(
            d2exp_evaluate(resolved(program, ctx)),
            D2Vtupl(fnlist_cons[d2val](
                D2Vstr("hello"),
                fnlist_cons[d2val](D2Vint(1), fnlist_nil()),
            )),
        )


class TwiceTests(unittest.TestCase):
    def test_twice_applies_a_function_twice(self) -> None:
        ctx = impl_ctx(twice_impl())
        for n in (0, 1, 5, 20):
            with self.subTest(n=n):
                # let f = lam y. y + 1; x = n in twice@[Nat -> Nat, Nat] end
                program = D2Elets(
                    fnlist_cons[d2ecl](
                        D2Cbind("f", D2Elam("y", con("Nat"),
                                            D2Eop1("+1", D2Evar("y")))),
                        fnlist_cons[d2ecl](
                            D2Cbind("x", D2Eint(n)),
                            fnlist_nil[d2ecl](),
                        ),
                    ),
                    fnoptn_cons[d2exp](D2Etapp(D2Ecst("twice"), s2lst([
                        S2Efun(con("Nat"), con("Nat")), con("Nat"),
                    ]))),
                )
                self.assertEqual(d2exp_evaluate(resolved(program, ctx)),
                                 D2Vint(n + 2))

    def test_function_type_argument_must_match(self) -> None:
        ctx = impl_ctx(twice_impl())
        use = D2Etapp(D2Ecst("twice"), s2lst([
            S2Efun(con("Nat"), con("Bool")), con("Nat"),
        ]))
        with self.assertRaises(TypeError):
            t2_replace(use, ctx)


class NestedTemplateTests(unittest.TestCase):
    def test_template_use_inside_a_template_is_expanded(self) -> None:
        ctx = impl_ctx(inc_impl(), inc2_impl(), inc3_impl())
        for name, times in (("inc2", 1), ("inc3", 2)):
            for n in (0, 4, 10):
                with self.subTest(template=name, n=n):
                    # let n = n in inc2@[Nat] end, or inc3@[Nat] end, whose
                    # body uses inc twice
                    program = D2Elets(
                        fnlist_cons[d2ecl](
                            D2Cbind("n", D2Eint(n)),
                            fnlist_nil[d2ecl](),
                        ),
                        fnoptn_cons[d2exp](
                            D2Etapp(D2Ecst(name), s2lst([con("Nat")])),
                        ),
                    )
                    self.assertEqual(d2exp_evaluate(resolved(program, ctx)),
                                     D2Vint(times * (n + 1)))


class FactorialTemplateTests(unittest.TestCase):
    def test_annotations_are_substituted_in_a_recursive_template(self) -> None:
        ctx = impl_ctx(fact_impl())
        nat = con("Nat")
        result = resolved(
            D2Eapp(D2Etapp(D2Ecst("fact"), s2lst([nat])), D2Eint(5)), ctx,
        )
        self.assertEqual(result, D2Eapp(
            D2Efix(
                "f", "n", nat,
                D2Eif0(
                    D2Eop2("==", D2Evar("n"), D2Eint(0)),
                    D2Eint(1),
                    D2Eop2(
                        "*", D2Evar("n"),
                        D2Eapp(D2Evar("f"),
                               D2Eop2("-", D2Evar("n"), D2Eint(1))),
                    ),
                ),
                S2Efun(nat, nat),
            ),
            D2Eint(5),
        ))

    def test_factorial(self) -> None:
        ctx = impl_ctx(fact_impl())
        for n in (0, 1, 2, 5, 10):
            with self.subTest(n=n):
                use = D2Eapp(D2Etapp(D2Ecst("fact"), s2lst([con("Nat")])),
                             D2Eint(n))
                self.assertEqual(d2exp_evaluate(resolved(use, ctx)),
                                 D2Vint(math.factorial(n)))


if __name__ == "__main__":
    unittest.main()
