"""Choosing implementations and expanding template uses."""

import unittest

from FWATS3_2basics import (
    D2E000, D2Eapp, D2Ecst, D2Efix, D2Eif0, D2Eint, D2Elam, D2Elets, D2Eop1,
    D2Eop2, D2Eproj, D2Etapp, D2Etupl, D2Estr, D2Evar,
    D2Cbind, D2Cimpl, D2Clocal,
    d2exp, d2expopt, d2explst, d2ecl, d2eclist,
    S2Econ, S2Efun, S2Evar, s2exp, s2explst, strn,
    fnlist_cons, fnlist_nil, fnoptn_cons, fnoptn_nil,
)
from FWATS3_2staexp import CTXnil, CTXcns, s2ctx
from FWATS3_2template import s2tmp_collect, t2_choose_impl, t2_replace


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


def zero_impl() -> D2Cimpl:
    """Build the template zero@Nat := 0, with a concrete parameter type."""
    return D2Cimpl("zero", D2Eint(0), fnlist_nil(), s2lst([con("Nat")]))


def identity_impl() -> D2Cimpl:
    """Build the template id@a := lam x. x, with a variable parameter type."""
    return D2Cimpl(
        "id", D2Elam("x", S2Evar("a"), D2Evar("x")),
        fnlist_cons[strn]("a", fnlist_nil()), s2lst([S2Evar("a")]),
    )


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


def template_uses(dexp: d2exp) -> list[D2Etapp]:
    """Collect every template use in the expression, in traversal order."""
    found: list[D2Etapp] = []

    def f0_d2eclist(decls: d2eclist) -> None:
        rest = decls
        while isinstance(rest, fnlist_cons):
            f0_d2ecl(rest.arg1)
            rest = rest.arg2

    def f0_d2ecl(decl: d2ecl) -> None:
        if isinstance(decl, D2Cbind):
            f0_d2exp(decl.arg2)
        elif isinstance(decl, D2Cimpl):
            f0_d2exp(decl.arg2)
        elif isinstance(decl, D2Clocal):
            f0_d2eclist(decl.arg1)
            f0_d2eclist(decl.arg2)

    def f0_d2expopt(body: d2expopt) -> None:
        if isinstance(body, fnoptn_cons):
            f0_d2exp(body.arg1)

    def f0_d2explst(d2es: d2explst) -> None:
        rest = d2es
        while isinstance(rest, fnlist_cons):
            f0_d2exp(rest.arg1)
            rest = rest.arg2

    def f0_d2exp(dexp: d2exp) -> None:
        if isinstance(dexp, D2Eop1):
            f0_d2exp(dexp.arg1)
        elif isinstance(dexp, D2Eop2):
            f0_d2exp(dexp.arg1)
            f0_d2exp(dexp.arg2)
        elif isinstance(dexp, D2Elam):
            f0_d2exp(dexp.arg3)
        elif isinstance(dexp, D2Efix):
            f0_d2exp(dexp.arg4)
        elif isinstance(dexp, D2Eapp):
            f0_d2exp(dexp.arg1)
            f0_d2exp(dexp.arg2)
        elif isinstance(dexp, D2Eif0):
            f0_d2exp(dexp.arg1)
            f0_d2exp(dexp.arg2)
            f0_d2exp(dexp.arg3)
        elif isinstance(dexp, D2Etupl):
            f0_d2explst(dexp.arg1)
        elif isinstance(dexp, D2Eproj):
            f0_d2exp(dexp.arg2)
        elif isinstance(dexp, D2Elets):
            f0_d2eclist(dexp.arg1)
            f0_d2expopt(dexp.arg2)
        elif isinstance(dexp, D2Etapp):
            found.append(dexp)
            f0_d2exp(dexp.arg1)

    f0_d2exp(dexp)
    return found


class TemplateContextTests(unittest.TestCase):
    def test_collect_binds_only_implementations(self) -> None:
        ctx = impl_ctx(zero_impl())
        found = s2tmp_collect(D2Cbind("x", D2Eint(0)), ctx)
        self.assertIsInstance(found, fnoptn_cons)
        assert isinstance(found, fnoptn_cons)
        self.assertIs(found.arg1, ctx)

    def test_collect_prepends_the_newest_implementation(self) -> None:
        first, second = zero_impl(), identity_impl()
        ctx = impl_ctx(first, second)
        self.assertIsInstance(ctx, CTXcns)
        assert isinstance(ctx, CTXcns)
        self.assertEqual(ctx.arg1, "id")
        self.assertIs(ctx.arg2, second)
        self.assertIsInstance(ctx.arg3, CTXcns)
        assert isinstance(ctx.arg3, CTXcns)
        self.assertEqual(ctx.arg3.arg1, "zero")
        self.assertIs(ctx.arg3.arg2, first)
        self.assertIsInstance(ctx.arg3.arg3, CTXnil)


class ImplementationChoiceTests(unittest.TestCase):
    def test_implementation_for_a_concrete_type_is_found(self) -> None:
        ctx = impl_ctx(zero_impl())
        use = D2Etapp(D2Ecst("zero"), s2lst([con("Nat")]))
        found = t2_choose_impl(use, ctx)
        self.assertIsInstance(found, fnoptn_cons)
        assert isinstance(found, fnoptn_cons)
        body: d2exp = found.arg1
        self.assertEqual(body, D2Eint(0))
        self.assertEqual(resolved(use, ctx), D2Eint(0))

    def test_implementation_over_a_variable_is_found(self) -> None:
        ctx = impl_ctx(identity_impl())
        for s2t in (con("Nat"), con("Bool"), arr(con("Nat"), con("Bool"))):
            with self.subTest(type=s2t):
                use = D2Eapp(D2Etapp(D2Ecst("id"), s2lst([s2t])), D2Eint(5))
                self.assertEqual(
                    resolved(use, ctx),
                    D2Eapp(D2Elam("x", s2t, D2Evar("x")), D2Eint(5)),
                )

    def test_concrete_implementation_rejects_another_type(self) -> None:
        ctx = impl_ctx(zero_impl())
        use = D2Etapp(D2Ecst("zero"), s2lst([con("Bool")]))
        self.assertIsInstance(t2_choose_impl(use, ctx), fnoptn_nil)

    def test_newest_declaration_for_a_name_wins(self) -> None:
        generic = D2Cimpl("pick", D2Evar("generic"), fnlist_nil(),
                          s2lst([S2Evar("a")]))
        concrete = D2Cimpl("pick", D2Evar("nat"), fnlist_nil(),
                           s2lst([con("Nat")]))
        # A use of pick@Nat picks the concrete implementation when it is
        # declared last, and the generic one when the generic implementation
        # is declared last.
        self.assertEqual(
            resolved(D2Etapp(D2Ecst("pick"), s2lst([con("Nat")])),
                     impl_ctx(generic, concrete)),
            D2Evar("nat"),
        )
        newest_generic = impl_ctx(concrete, generic)
        for s2t in (con("Nat"), con("Bool")):
            with self.subTest(type=s2t):
                self.assertEqual(
                    resolved(D2Etapp(D2Ecst("pick"), s2lst([s2t])),
                             newest_generic),
                    D2Evar("generic"),
                )

    def test_implementation_falls_through_to_an_older_declaration(self) -> None:
        generic = D2Cimpl("pick", D2Evar("generic"), fnlist_nil(),
                          s2lst([S2Evar("a")]))
        concrete = D2Cimpl("pick", D2Evar("nat"), fnlist_nil(),
                           s2lst([con("Nat")]))
        # The newest implementation for pick does not take Bool, so the older
        # generic implementation is used instead.
        ctx = impl_ctx(generic, concrete)
        self.assertEqual(
            resolved(D2Etapp(D2Ecst("pick"), s2lst([con("Nat")])), ctx),
            D2Evar("nat"),
        )
        self.assertEqual(
            resolved(D2Etapp(D2Ecst("pick"), s2lst([con("Bool")])), ctx),
            D2Evar("generic"),
        )

    def test_implementation_is_found_past_another_declaration(self) -> None:
        ctx = impl_ctx(zero_impl(), identity_impl())
        self.assertEqual(
            resolved(D2Etapp(D2Ecst("zero"), s2lst([con("Nat")])), ctx),
            D2Eint(0),
        )

    def test_use_with_a_non_constant_name_is_unresolved(self) -> None:
        ctx = impl_ctx(identity_impl())
        use = D2Etapp(D2Evar("id"), s2lst([con("Nat")]))
        self.assertIsInstance(t2_choose_impl(use, ctx), fnoptn_nil)

    def test_use_of_an_unknown_template_is_unresolved(self) -> None:
        ctx = impl_ctx(identity_impl())
        use = D2Etapp(D2Ecst("missing"), s2lst([con("Nat")]))
        self.assertIsInstance(t2_choose_impl(use, ctx), fnoptn_nil)


class TemplateExpansionTests(unittest.TestCase):
    def test_successful_replacement_leaves_no_template_use(self) -> None:
        pair = D2Cimpl("pair", D2Etupl(d2lst([D2Evar("x"), D2Evar("y")])),
                       fnlist_nil(), s2lst([S2Evar("a"), S2Evar("b")]))
        swap = D2Cimpl(
            "swap",
            D2Etupl(d2lst([D2Eproj(1, D2Evar("p")), D2Eproj(0, D2Evar("p"))])),
            fnlist_cons[strn]("a", fnlist_nil()), s2lst([S2Evar("a")]),
        )
        ctx = impl_ctx(pair, swap)
        text = con("Str")
        # let x = 1; y = "hello"; p = pair@[Nat,Str]
        # in swap@[Arr(Nat,Str)](p) end
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
            fnoptn_cons[d2exp](D2Eapp(
                D2Etapp(D2Ecst("swap"), s2lst([arr(con("Nat"), text)])),
                D2Evar("p"),
            )),
        )
        result = resolved(program, ctx)
        self.assertEqual(template_uses(result), [])
        self.assertEqual(result, D2Elets(
            fnlist_cons[d2ecl](
                D2Cbind("x", D2Eint(1)),
                fnlist_cons[d2ecl](
                    D2Cbind("y", D2Estr("hello")),
                    fnlist_cons[d2ecl](
                        D2Cbind("p", D2Etupl(d2lst([D2Evar("x"),
                                                    D2Evar("y")]))),
                        fnlist_nil[d2ecl](),
                    ),
                ),
            ),
            fnoptn_cons[d2exp](D2Eapp(
                D2Etupl(d2lst([D2Eproj(1, D2Evar("p")),
                               D2Eproj(0, D2Evar("p"))])),
                D2Evar("p"),
            )),
        ))

    def test_nested_template_uses_are_expanded(self) -> None:
        inner = D2Cimpl("inc", D2Eop1("+1", D2Evar("n")), fnlist_nil(),
                        s2lst([con("Nat")]))
        outer = D2Cimpl("inc2", D2Etapp(D2Ecst("inc"), s2lst([S2Evar("a")])),
                        fnlist_nil(), s2lst([S2Evar("a")]))
        ctx = impl_ctx(inner, outer)
        use = D2Eapp(D2Etapp(D2Ecst("inc2"), s2lst([con("Nat")])),
                     D2Evar("n"))
        result = resolved(use, ctx)
        self.assertEqual(template_uses(result), [])
        self.assertEqual(result, D2Eapp(D2Eop1("+1", D2Evar("n")),
                                        D2Evar("n")))

    def test_substitution_is_applied_to_annotations_in_the_body(self) -> None:
        a = S2Evar("a")
        impl = D2Cimpl(
            "annot",
            D2Elam("x", a, D2Efix(
                "f", "n", a,
                D2Eif0(
                    D2Eop2("==", D2Evar("n"), D2Eint(0)),
                    D2Evar("x"),
                    D2Eapp(D2Evar("f"),
                           D2Eop2("-", D2Evar("n"), D2Eint(1))),
                ),
                S2Efun(a, a),
            )),
            fnlist_cons[strn]("a", fnlist_nil()), s2lst([a]),
        )
        ctx = impl_ctx(impl)
        for s2t in (con("Nat"), arr(con("Nat"), con("Bool"))):
            with self.subTest(type=s2t):
                result = resolved(D2Etapp(D2Ecst("annot"), s2lst([s2t])), ctx)
                self.assertEqual(result, D2Elam("x", s2t, D2Efix(
                    "f", "n", s2t,
                    D2Eif0(
                        D2Eop2("==", D2Evar("n"), D2Eint(0)),
                        D2Evar("x"),
                        D2Eapp(D2Evar("f"),
                               D2Eop2("-", D2Evar("n"), D2Eint(1))),
                    ),
                    S2Efun(s2t, s2t),
                )))

    def test_unbound_type_variables_are_kept(self) -> None:
        impl = D2Cimpl(
            "keep",
            D2Elam("x", S2Evar("a"),
                   D2Elam("y", S2Evar("b"), D2Evar("x"))),
            fnlist_nil(), s2lst([S2Evar("a")]),
        )
        ctx = impl_ctx(impl)
        result = resolved(D2Etapp(D2Ecst("keep"), s2lst([con("Nat")])), ctx)
        self.assertEqual(result, D2Elam("x", con("Nat"),
                                        D2Elam("y", S2Evar("b"),
                                               D2Evar("x"))))

    def test_declarations_in_a_let_are_expanded(self) -> None:
        ctx = impl_ctx(zero_impl())
        # let a = zero@[Nat]; b = f(0); unused = zero@[Nat] in b end,
        # where the implementation body is expanded but its signature is not
        # rewritten, because no substitution applies to a traversed
        # implementation.
        program = D2Elets(
            fnlist_cons[d2ecl](
                D2Cbind("a", D2Etapp(D2Ecst("zero"), s2lst([con("Nat")]))),
                fnlist_cons[d2ecl](
                    D2Cbind("b", D2Eapp(D2Evar("f"), D2Eint(0))),
                    fnlist_cons[d2ecl](
                        D2Cimpl("unused",
                                D2Etapp(D2Ecst("zero"), s2lst([con("Nat")])),
                                fnlist_nil(), s2lst([S2Evar("z")])),
                        fnlist_nil[d2ecl](),
                    ),
                ),
            ),
            fnoptn_cons[d2exp](D2Evar("b")),
        )
        result = resolved(program, ctx)
        self.assertEqual(template_uses(result), [])
        self.assertEqual(result, D2Elets(
            fnlist_cons[d2ecl](
                D2Cbind("a", D2Eint(0)),
                fnlist_cons[d2ecl](
                    D2Cbind("b", D2Eapp(D2Evar("f"), D2Eint(0))),
                    fnlist_cons[d2ecl](
                        D2Cimpl("unused", D2Eint(0), fnlist_nil(),
                                s2lst([S2Evar("z")])),
                        fnlist_nil[d2ecl](),
                    ),
                ),
            ),
            fnoptn_cons[d2exp](D2Evar("b")),
        ))

    def test_expression_without_template_uses_is_rebuilt(self) -> None:
        expression = D2Eop2("+", D2Evar("n"), D2Eint(1))
        result = resolved(expression, CTXnil())
        self.assertEqual(result, expression)
        self.assertIsNot(result, expression)

    def test_placeholder_expression_has_no_result(self) -> None:
        self.assertIsInstance(t2_replace(D2E000(), CTXnil()), fnoptn_nil)

    def test_unresolved_use_raises(self) -> None:
        for use in (D2Etapp(D2Ecst("missing"), s2lst([con("Nat")])),
                    D2Etapp(D2Evar("missing"), s2lst([con("Nat")]))):
            with self.subTest(use=use):
                with self.assertRaises(TypeError):
                    t2_replace(use, CTXnil())


if __name__ == "__main__":
    unittest.main()
