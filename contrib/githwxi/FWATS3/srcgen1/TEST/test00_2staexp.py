"""Structural equality for level-2 types."""

import unittest

from FWATS3_2basics import (
    S2E000, S2Econ, S2Evar, S2Efun, S2Etupl,
    CTXnil, CTXcns,
    s2exp, s2explst, fnlist_cons, fnlist_nil,
    fnoptn_cons, fnoptn_nil,
)
from FWATS3_2staexp import s2exp_equal, s2exp_match


def types(*items: s2exp) -> s2explst:
    result: s2explst = fnlist_nil()
    for item in reversed(items):
        result = fnlist_cons(item, result)
    return result


class StructuralEqualityTests(unittest.TestCase):
    def test_equal_separately_allocated_nested_types(self) -> None:
        def nested() -> s2exp:
            return S2Efun(
                S2Econ("list", types(S2Evar("a"))),
                S2Etupl(types(S2Evar("a"), S2Econ("int", types()))),
            )
        self.assertTrue(s2exp_equal(nested(), nested()))

    def test_empty_types_and_placeholders(self) -> None:
        for left, right in (
            (S2E000(), S2E000()),
            (S2Etupl(types()), S2Etupl(types())),
            (S2Econ("int", types()), S2Econ("int", types())),
        ):
            self.assertTrue(s2exp_equal(left, right))

    def test_differences_in_kind_name_children_order_and_length(self) -> None:
        a, b = S2Evar("a"), S2Evar("b")
        pairs: list[tuple[s2exp, s2exp]] = [
            (a, b),
            (a, S2Econ("a", types())),
            (S2E000(), a),
            (S2Econ("int", types()), S2Econ("bool", types())),
            (S2Econ("list", types(a)), S2Econ("list", types(b))),
            (S2Econ("pair", types(a, b)), S2Econ("pair", types(b, a))),
            (S2Econ("list", types(a)), S2Econ("list", types())),
            (S2Efun(a, a), S2Efun(b, a)),
            (S2Efun(a, a), S2Efun(a, b)),
            (S2Etupl(types(a, b)), S2Etupl(types(b, a))),
            (S2Etupl(types(a)), S2Etupl(types(a, b))),
            (S2Etupl(types()), S2Etupl(types(a))),
        ]
        for left, right in pairs:
            with self.subTest(left=left, right=right):
                self.assertFalse(s2exp_equal(left, right))
                self.assertFalse(s2exp_equal(right, left))


class TypeMatchingTests(unittest.TestCase):
    def test_nested_matching_collects_consistent_bindings(self) -> None:
        a, b = S2Evar("a"), S2Evar("b")
        integer = S2Econ("int", types())
        pattern = S2Efun(S2Econ("list", types(a)), S2Etupl(types(b, a)))
        target = S2Efun(
            S2Econ("list", types(integer)),
            S2Etupl(types(S2Evar("x"), S2Econ("int", types()))),
        )
        self.assertEqual(
            s2exp_match(pattern, target),
            fnoptn_cons(CTXcns("b", S2Evar("x"), CTXcns("a", integer, CTXnil()))),
        )

    def test_success_without_bindings(self) -> None:
        for typ in (S2E000(), S2Etupl(types()), S2Econ("int", types())):
            self.assertEqual(s2exp_match(typ, typ), fnoptn_cons(CTXnil()))

    def test_mismatches(self) -> None:
        a = S2Evar("a")
        integer, boolean = S2Econ("int", types()), S2Econ("bool", types())
        pairs: list[tuple[s2exp, s2exp]] = [
            (integer, boolean),
            (integer, a),
            (S2E000(), integer),
            (S2Efun(a, a), S2Efun(integer, boolean)),
            (S2Efun(a, a), S2Etupl(types(integer, integer))),
            (S2Econ("list", types(a)), S2Econ("list", types())),
            (S2Etupl(types()), S2Etupl(types(integer))),
            (S2Etupl(types(a, a)), S2Etupl(types(integer))),
            (S2Etupl(types(a, a)), S2Etupl(types(S2Evar("x"), S2Evar("y")))),
        ]
        for pattern, target in pairs:
            with self.subTest(pattern=pattern, target=target):
                self.assertEqual(s2exp_match(pattern, target), fnoptn_nil())

    def test_target_variables_are_literal_and_calls_are_independent(self) -> None:
        a = S2Evar("a")
        for target in (a, S2Econ("list", types(a)), S2Econ("int", types())):
            self.assertEqual(
                s2exp_match(a, target), fnoptn_cons(CTXcns("a", target, CTXnil())),
            )


if __name__ == "__main__":
    unittest.main()
