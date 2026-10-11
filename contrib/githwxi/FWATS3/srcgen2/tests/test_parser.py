"""Behavioral tests for the FWATS3 concrete syntax."""
from pathlib import Path
import unittest

import FWATS3_2basics as a
from FWATS3_2parser import (
    FWATS3SyntaxError, d2exp_parse, s2exp_parse, libfile_parse, program_parse,
)


def seq(*values):
    result = a.fnlist_nil()
    for value in reversed(values):
        result = a.fnlist_cons(value, result)
    return result


def con(name):
    return a.S2Econ(name, seq())


class ParserTests(unittest.TestCase):
    def test_type_grouping_and_arrows(self):
        var = a.S2Evar("a")
        self.assertEqual(s2exp_parse("((a))", static_vars={"a"}), var)
        self.assertEqual(s2exp_parse("(a,)", static_vars={"a"}), a.S2Etupl(seq(var)))
        self.assertEqual(s2exp_parse("()"), a.S2Etupl(seq()))
        self.assertEqual(s2exp_parse("a"), con("a"))
        self.assertEqual(s2exp_parse("int[]"), con("int"))
        self.assertEqual(s2exp_parse("a -> b -> c"),
                         a.S2Efun(con("a"), a.S2Efun(con("b"), con("c"))))
        self.assertEqual(s2exp_parse("list[a,]", static_vars={"a"}),
                         a.S2Econ("list", seq(var)))

    def test_parenthesized_type_constructors(self):
        for source, equivalent in (
            ("list(sint)", "list[sint]"),
            ("strm(a)", "strm[a]"),
            ("list(strm(a))", "list[strm[a]]"),
            ("strm((a, sint))", "strm[(a,sint)]"),
            ("list(a -> sint)", "list[a -> sint]"),
            ("strm(a,)", "strm[a]"),
        ):
            with self.subTest(source=source):
                self.assertEqual(s2exp_parse(source, static_vars={"a"}),
                                 s2exp_parse(equivalent, static_vars={"a"}))
        with self.assertRaises(FWATS3SyntaxError):
            s2exp_parse("a(sint)", static_vars={"a"})

    def test_precedence_and_associativity(self):
        self.assertEqual(d2exp_parse("-1 + 2 * 3 - 4 / +2 < 9"),
            a.D2Eop2("<",
                a.D2Eop2("-",
                    a.D2Eop2("+", a.D2Eop1("-", a.D2Eint(1)),
                             a.D2Eop2("*", a.D2Eint(2), a.D2Eint(3))),
                    a.D2Eop2("/", a.D2Eint(4), a.D2Eop1("+", a.D2Eint(2)))),
                a.D2Eint(9)))
        for op in ("<", ">", "<=", ">=", "==", "!="):
            with self.subTest(op=op):
                self.assertEqual(d2exp_parse(f"x {op} y"),
                                 a.D2Eop2(op, a.D2Evar("x"), a.D2Evar("y")))

    def test_modulo_precedence_and_associativity(self):
        self.assertEqual(d2exp_parse("1+20%6*2/4"),
            a.D2Eop2("+", a.D2Eint(1),
                a.D2Eop2("/",
                    a.D2Eop2("*",
                        a.D2Eop2("%", a.D2Eint(20), a.D2Eint(6)),
                        a.D2Eint(2)),
                    a.D2Eint(4))))
        self.assertEqual(d2exp_parse("20*3%7%2"),
            a.D2Eop2("%",
                a.D2Eop2("%", a.D2Eop2("*", a.D2Eint(20), a.D2Eint(3)),
                         a.D2Eint(7)),
                a.D2Eint(2)))
        self.assertEqual(d2exp_parse("-7 % +3"),
            a.D2Eop2("%", a.D2Eop1("-", a.D2Eint(7)),
                     a.D2Eop1("+", a.D2Eint(3))))
        implementation = libfile_parse(
            "imp f1_mod[sint] = lam(xy: (sint, sint)) => xy.0 % xy.1;"
        ).arg1
        self.assertEqual(implementation.arg2.arg3,
            a.D2Eop2("%", a.D2Eproj(0, a.D2Evar("xy")),
                     a.D2Eproj(1, a.D2Evar("xy"))))

    def test_calls_tuples_and_postfix_order(self):
        self.assertEqual(d2exp_parse("@f[int](x).0(y)"),
            a.D2Eapp(a.D2Eproj(0, a.D2Eapp(
                a.D2Etapp(a.D2Ecst("f"), seq(con("int"))), a.D2Evar("x"))),
                a.D2Evar("y")))
        for source, argument in [
            ("f()", a.D2Etupl(seq())),
            ("f(x)", a.D2Evar("x")),
            ("f(x,)", a.D2Etupl(seq(a.D2Evar("x")))),
            ("f(x,y,)", a.D2Etupl(seq(a.D2Evar("x"), a.D2Evar("y")))),
        ]:
            self.assertEqual(d2exp_parse(source), a.D2Eapp(a.D2Evar("f"), argument))
        self.assertEqual(d2exp_parse("f[]"), a.D2Etapp(a.D2Evar("f"), seq()))

    def test_lexical_rules(self):
        self.assertEqual(d2exp_parse(' "a#b\\n\\t\\r\\\\\\\"" # comment\n'),
                         a.D2Estr('a#b\n\t\r\\"'))
        self.assertEqual(d2exp_parse("@foo$bar"), a.D2Ecst("foo$bar"))
        self.assertEqual(d2exp_parse("true"), a.D2Ebtf(True))
        self.assertEqual(d2exp_parse("false"), a.D2Ebtf(False))
        self.assertEqual(d2exp_parse("true$"), a.D2Evar("true$"))

    def test_signature_and_tuple_pattern(self):
        result = program_parse("""
            dec identity[t]: t -> t;
            imp{a,b} identity[(a,b)] = lam(x: (a,b)) => x;
            @identity[(int,bool)](42, true)
        """)
        pair = a.S2Etupl(seq(a.S2Evar("a"), a.S2Evar("b")))
        self.assertEqual(result.arg1, seq(
            a.D2Cdecl("identity", seq("t"), a.S2Efun(a.S2Evar("t"), a.S2Evar("t"))),
            a.D2Cimpl("identity", a.D2Elam("x", pair, a.D2Evar("x")),
                      seq("a", "b"), seq(pair))))
        self.assertIsInstance(result.arg2, a.fnoptn_cons)
        self.assertIsInstance(result.arg2.arg1, a.D2Eapp)

    def test_scopes_are_restored_and_outer_parameters_are_visible(self):
        decls = libfile_parse("""
            imp{a} outer[a] = let
                dec inner[b]: (a,b) -> a;
                imp{b} inner[b] = lam(x: a) => x
            in lam(x: a) => x end;
            dec plain: a;
            imp plain = lam(x: a) => x
        """)
        outer = decls.arg1
        inner_signature = outer.arg2.arg1.arg1.arg3
        self.assertEqual(inner_signature.arg1,
                         a.S2Etupl(seq(a.S2Evar("a"), a.S2Evar("b"))))
        self.assertEqual(decls.arg2.arg1.arg3, con("a"))
        self.assertEqual(decls.arg2.arg2.arg1.arg2.arg2, con("a"))

    def test_fix_if_and_let(self):
        result = d2exp_parse("fix f(n: int): int => if n > 0 then f(n-1) else 1")
        self.assertIsInstance(result, a.D2Efix)
        self.assertEqual((result.arg1, result.arg2, result.arg3, result.arg5),
                         ("f", "n", con("int"), con("int")))
        self.assertIsInstance(result.arg4, a.D2Eif0)
        self.assertEqual(d2exp_parse("let end"), a.D2Elets(seq(), a.fnoptn_nil()))
        self.assertEqual(d2exp_parse("let in () end"),
                         a.D2Elets(seq(), a.fnoptn_cons(a.D2Etupl(seq()))))
        self.assertEqual(d2exp_parse("let val x = 1 in x end"),
                         a.D2Elets(seq(a.D2Cbind("x", a.D2Eint(1))),
                                   a.fnoptn_cons(a.D2Evar("x"))))

    def test_local_and_empty_lists(self):
        self.assertEqual(libfile_parse("local in end"),
                         seq(a.D2Clocal(seq(), seq())))
        self.assertEqual(libfile_parse("dec x[]: int; imp x[] = 1;"),
                         seq(a.D2Cdecl("x", seq(), con("int")),
                             a.D2Cimpl("x", a.D2Eint(1), seq(), seq())))

    def test_program_library_boundary(self):
        self.assertEqual(libfile_parse(" # empty"), seq())
        self.assertEqual(program_parse("42"),
                         a.D2Elets(seq(), a.fnoptn_cons(a.D2Eint(42))))
        self.assertEqual(program_parse("val x = 1; x"),
                         program_parse("val x = 1 x"))
        for source in ("", "val x = 1", "42;", "42 43"):
            with self.subTest(source=source), self.assertRaises(FWATS3SyntaxError):
                program_parse(source)
        with self.assertRaises(FWATS3SyntaxError):
            libfile_parse("val x = 1; x")

    def test_actual_library(self):
        path = Path(__file__).resolve().parents[1] / "mylib" / "1.fwats3"
        declarations = libfile_parse(path.read_text(), filename=str(path))
        values = []
        while isinstance(declarations, a.fnlist_cons):
            values.append(declarations.arg1)
            declarations = declarations.arg2
        signatures = {x.arg1: x for x in values if isinstance(x, a.D2Cdecl)}
        implementations = {x.arg1: x for x in values if isinstance(x, a.D2Cimpl)}
        self.assertEqual(implementations["f1_add"].arg2.arg2,
                         a.S2Etupl(seq(con("sint"), con("sint"))))
        self.assertEqual(implementations["f1_sub"].arg2.arg3.name, "-")
        for name in ("f1_fact", "f1_fibo"):
            self.assertEqual(signatures[name].arg3,
                             a.S2Efun(a.S2Evar("a"), a.S2Evar("a")))
            implementation = implementations[name]
            self.assertEqual(implementation.arg3, seq("a"))
            self.assertEqual(implementation.arg4, seq(a.S2Evar("a")))
            self.assertIsInstance(implementation.arg2, a.D2Elets)
            recursive = implementation.arg2.arg2.arg1
            self.assertIsInstance(recursive, a.D2Efix)
            self.assertEqual(recursive.arg3, a.S2Evar("a"))
            self.assertEqual(recursive.arg5, a.S2Evar("a"))

    def test_invalid_expressions(self):
        for source in (
            "lam(x) => x", "fix f(x: int) => x", "fix x(x: int): int => x",
            "if true then 1", "1 < 2 < 3", "x.-1", "x.foo", "f(,)", "(1,,)",
            "let in end", "1 + lam(x: int) => x", "$x", "'a", '"bad\\q"',
            '"unterminated', '"line\nbreak"', "x y", "1 %", "% 2", "1 %% 2", "@if",
        ):
            with self.subTest(source=source), self.assertRaises(FWATS3SyntaxError):
                d2exp_parse(source)

    def test_invalid_declarations_and_types(self):
        for source in (
            "val x = 1 val y = 2", "dec x: int dec y: int",
            "imp{} f = 1", "imp{a,a} f[a] = 1", "dec f[a,a]: a",
            "imp @f = 1", "imp f{a}[a] = 1", "dec f[a]: a[int]",
            "dec f[a]: a[]", "local val x = 1 end", ";", "val x = 1;;",
        ):
            with self.subTest(source=source), self.assertRaises(FWATS3SyntaxError):
                libfile_parse(source)
        for source in ("int int", "(int,,)", "list[,]", "int ->"):
            with self.subTest(source=source), self.assertRaises(FWATS3SyntaxError):
                s2exp_parse(source)

    def test_error_location(self):
        with self.assertRaises(FWATS3SyntaxError) as caught:
            libfile_parse("val x = 1;\nval y = $bad", filename="bad.fwats3")
        error = caught.exception
        self.assertEqual((error.filename, error.lineno, error.offset),
                         ("bad.fwats3", 2, 9))
        self.assertEqual(error.text, "val y = $bad")


if __name__ == "__main__":
    unittest.main()
