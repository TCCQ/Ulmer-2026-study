"""Type-checker acceptance, rejection, and scope regressions."""
from pathlib import Path
import unittest

import FWATS3_2basics as a
from FWATS3_2parser import d2exp_parse, libfile_parse, program_parse
from FWATS3_2tcheck import (
    BOOL, SINT, STRING, UNIT, FWATS3TypeError, TypeEnvironment,
    d2exp_tcheck, libfile_tcheck, program_tcheck,
)


def check(source):
    return program_tcheck(program_parse(source))


class TypeCheckerTests(unittest.TestCase):
    def test_list_and_stream_type_constructors(self):
        from FWATS3_2parser import s2exp_parse
        for constructor in ("list", "strm"):
            source = f"""
                dec id[a]: {constructor}(a) -> {constructor}(a);
                imp{{a}} id[a] = lam(xs: {constructor}(a)) => xs;
                @id[sint]
            """
            self.assertEqual(check(source),
                s2exp_parse(f"{constructor}(sint) -> {constructor}(sint)"))
            for bad in (constructor, f"{constructor}()", f"{constructor}(sint,bool)"):
                with self.subTest(bad=bad), self.assertRaises(FWATS3TypeError):
                    libfile_tcheck(libfile_parse(f"dec bad: {bad};"))
        with self.assertRaises(FWATS3TypeError):
            check("dec bad[a]: list(a) -> strm(a); "
                  "imp{a} bad[a] = lam(xs: list(a)) => xs; 0")

    def test_literals_and_operators(self):
        for source, expected in [
            ("42", SINT), ('"hello"', STRING), ("true", BOOL), ("()", UNIT),
            ("-7 % +3", SINT), ("1 < 2", BOOL), ("false != true", BOOL),
            ('"a" == "b"', BOOL),
            ("(lam(x: int) => x + 1)(2)", SINT),
        ]:
            with self.subTest(source=source):
                self.assertEqual(check(source), expected)

    def test_functions_tuples_recursion_and_let(self):
        self.assertEqual(check("""
            val f = fix f(n: sint): sint =>
                if n <= 1 then 1 else n * f(n-1);
            let val pair = (f(5), true) in pair.0 end
        """), SINT)
        self.assertEqual(check("let val x = 1 end"), UNIT)
        self.assertEqual(check("val x = 1; val x = x + 1; x"), SINT)
        self.assertEqual(check("(lam(f: sint -> bool) => f(1))(lam(x: sint) => true)"), BOOL)

    def test_generic_and_specialized_implementations(self):
        self.assertEqual(check("""
            dec id[a]: a -> a;
            imp{b} id[b] = lam(x: b) => x;
            imp id[sint] = lam(x: sint) => x;
            imp{x,y} id[(x,y)] = lam(pair: (x,y)) => pair;
            @id[(sint,bool)](1, true).1
        """), BOOL)
        self.assertEqual(check("dec answer: sint; imp answer = 42; @answer"), SINT)
        self.assertEqual(check("dec answer: sint; @answer[]"), SINT)
        # A declaration suffices: availability/selection of implementations is separate.
        self.assertEqual(check("dec f[a]: a -> a; @f[string](\"ok\")"), STRING)

    def test_library(self):
        root = Path(__file__).resolve().parents[1]
        env = libfile_tcheck(libfile_parse((root / "mylib/1.fwats3").read_text()))
        for name in ("f1_fact", "f1_fibo"):
            self.assertEqual(d2exp_tcheck(d2exp_parse(f"@{name}[sint](5)"), env), SINT)
            self.assertIn(name, env.implementations)

    def test_local_exports_and_captures(self):
        env = libfile_tcheck(libfile_parse("""
            local
                val hidden = 1;
                dec helper: sint;
                imp helper = hidden
            in
                val exposed = lam(u: ()) => @helper;
                dec public: () -> sint;
                imp public = exposed
            end
        """))
        self.assertNotIn("hidden", env.variables)
        self.assertNotIn("helper", env.constants)
        self.assertNotIn("helper", env.implementations)
        self.assertIn("public", env.implementations)
        self.assertEqual(d2exp_tcheck(d2exp_parse("@public()"), env), SINT)
        self.assertEqual(check("""
            val x = 1;
            local val x = true in val y = x end;
            (x,y).1
        """), BOOL)

    def test_nested_static_binders_do_not_capture(self):
        self.assertEqual(check("""
            dec outer[a]: a -> a;
            imp{a} outer[a] = lam(x: a) =>
                let
                    dec inner[a]: a -> a;
                    imp{a} inner[a] = lam(y: a) => y
                in @inner[a](x) end;
            @outer[sint](1)
        """), SINT)
        with self.assertRaises(FWATS3TypeError):
            check("""
                dec outer[a]: a -> a;
                imp{a} outer[a] = lam(x: a) =>
                    let
                        dec inner[a]: a -> a;
                        imp{a} inner[a] = lam(y: a) => x
                    in x end;
                0
            """)

    def test_failed_checks_do_not_mutate_environment(self):
        env = libfile_tcheck(libfile_parse("val x = 1; dec f[a]: a -> a;"))
        with self.assertRaises(FWATS3TypeError):
            libfile_tcheck(libfile_parse("val y = 2; imp f[sint] = true;"), env)
        self.assertNotIn("y", env.variables)
        self.assertEqual(env.implementations, {})
        extended = libfile_tcheck(libfile_parse("val y = x;"), env)
        self.assertIn("y", extended.variables)
        self.assertNotIn("y", env.variables)

    def test_expression_errors(self):
        for source in (
            "x", "@unknown", "1(true)", "(lam(x: sint) => x)(false)",
            "if 1 then 2 else 3", "if true then 1 else false",
            "(1,).1", "1.0", "true + 1", "1 % false", "-true",
            "(lam(x: sint) => x) == (lam(x: sint) => x)",
            "fix f(x: sint): bool => x",
            "let val x = x in x end",
            "val f = lam(x: sint) => x; f[sint](1)",
            "let val x = 1 in x end; x",
            "lam(x: list) => x",
        ):
            with self.subTest(source=source), self.assertRaises((FWATS3TypeError, SyntaxError)):
                check(source)

    def test_declaration_errors(self):
        for source in (
            "imp missing = 1;",
            "dec f[a]: a -> a; imp f[sint] = lam(x: bool) => x;",
            "dec f[a]: a -> a; imp{a} f[a] = lam(x: a) => 1;",
            "dec f[a]: a -> a; imp{a} f[a] = lam(x: a) => x + 1;",
            "dec f[a]: a -> a; imp f = lam(x: sint) => x;",
            "dec f[a]: a -> a; imp f[sint,bool] = 1;",
            "dec f[a]: a -> a; imp{b} f[sint] = lam(x: sint) => x;",
            "dec f: sint; dec f: bool;",
            "dec f: list[sint,bool];",
        ):
            with self.subTest(source=source), self.assertRaises(FWATS3TypeError):
                libfile_tcheck(libfile_parse(source))
        for expression in ("@f", "@f[]", "@f[sint,bool]", "@f[sint](false)"):
            env = libfile_tcheck(libfile_parse("dec f[a]: a -> a;"))
            with self.subTest(expression=expression), self.assertRaises(FWATS3TypeError):
                d2exp_tcheck(d2exp_parse(expression), env)

    def test_direct_ast_validation(self):
        for expression in (
            a.D2E000(),
            a.D2Elam("x", a.S2E000(), a.D2Evar("x")),
            a.D2Elam("x", a.S2Evar("free"), a.D2Evar("x")),
            a.D2Eop1("unknown", a.D2Eint(1)),
            a.D2Eop2("unknown", a.D2Eint(1), a.D2Eint(2)),
            a.D2Eproj(-1, a.D2Etupl(a.fnlist_nil())),
        ):
            with self.subTest(expression=expression), self.assertRaises(FWATS3TypeError):
                d2exp_tcheck(expression)
        with self.assertRaises(FWATS3TypeError):
            program_tcheck(d2exp_parse("let end"))


if __name__ == "__main__":
    unittest.main()
