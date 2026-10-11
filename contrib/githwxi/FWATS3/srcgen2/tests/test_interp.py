"""Interpreter tests emphasizing lexical capture of both environments."""
from pathlib import Path
import unittest

import FWATS3_2basics as a
from FWATS3_2parser import d2exp_parse, libfile_parse, program_parse
from FWATS3_2interp import (
    D2Vlam, D2Vfix, TemplateClosure, FWATS3RuntimeError,
    d2exp_evaluate, libfile_evaluate, program_evaluate,
)
from FWATS3_2tcheck import libfile_tcheck, program_tcheck


def run(source):
    tree = program_parse(source)
    program_tcheck(tree)
    return program_evaluate(tree)


class InterpreterTests(unittest.TestCase):
    def test_library_factorial_fibonacci_and_modulo(self):
        root = Path(__file__).resolve().parents[1]
        library = libfile_parse((root / "mylib/1.fwats3").read_text())
        libfile_tcheck(library)
        d, t = libfile_evaluate(library)
        for name, cases in (
            ("f1_fact", [(0, 1), (1, 1), (5, 120), (8, 40320)]),
            ("f1_fibo", [(0, 0), (1, 1), (2, 1), (10, 55)]),
        ):
            for argument, expected in cases:
                with self.subTest(name=name, argument=argument):
                    self.assertEqual(d2exp_evaluate(
                        d2exp_parse(f"@{name}[sint]({argument})"), d, t), expected)
        self.assertEqual(d2exp_evaluate(d2exp_parse("@f1_mod[sint](17,5)"), d, t), 2)
        self.assertEqual(run((root / "examples/factorial.fwats3").read_text()), 120)

    def test_primitive_values_and_control_flow(self):
        for source, expected in (
            ("()", ()), ("(42,)", (42,)), ('(1,true,"ok").2', "ok"),
            ("-7 / 3", -3), ("-7 % 3", 2), ("+2*3+1", 7),
            ('"a" != "b"', True), ("false == false", True),
            ("if true then 42 else 1/0", 42),
            ("if false then 1/0 else 42", 42),
            ("let val x = 1 end", ()),
            ("val x = 1; val x = x + 2; x", 3),
        ):
            with self.subTest(source=source):
                self.assertEqual(run(source), expected)

    def test_lambda_captures_both_environments(self):
        self.assertEqual(run("""
            dec value: sint;
            imp value = 10;
            val x = 1;
            val f = lam(u: ()) => x + @value;
            val x = 100;
            imp value = 20;
            (f(), @value, x)
        """), (11, 20, 100))

    def test_fix_captures_both_environments(self):
        self.assertEqual(run("""
            dec step: sint;
            imp step = 2;
            val initial = 10;
            val f = fix loop(n: sint): sint =>
                if n == 0 then initial else @step + loop(n-1);
            val initial = 100;
            imp step = 20;
            f(3)
        """), 16)

    def test_implementation_captures_both_environments(self):
        self.assertEqual(run("""
            dec helper: sint;
            imp helper = 10;
            val x = 1;
            dec f[a]: a -> sint;
            imp{a} f[a] = lam(ignored: a) => x + @helper;
            val x = 100;
            imp helper = 20;
            @f[bool](true)
        """), 11)

    def test_closure_records_and_environment_snapshots(self):
        incoming = {"x": 3}
        tree = libfile_parse("val f = lam(u: ()) => x; imp value = x;")
        d, t = libfile_evaluate(tree, incoming)
        incoming["x"] = 100
        self.assertIsInstance(d["f"], D2Vlam)
        self.assertIsInstance(t[0], TemplateClosure)
        self.assertEqual(d["f"].denv["x"], 3)
        self.assertEqual(t[0].denv["x"], 3)
        self.assertEqual(t[0].tenv, ())
        self.assertEqual(d2exp_evaluate(d2exp_parse("f()"), d, t), 3)
        self.assertEqual(d2exp_evaluate(d2exp_parse("@value"), d, t), 3)
        self.assertNotIn("f", incoming)
        with self.assertRaises(TypeError):
            d["x"] = 4
        self.assertIsInstance(d2exp_evaluate(
            d2exp_parse("fix f(n: sint): sint => n")), D2Vfix)

    def test_repeated_patterns_and_latest_matching_selection(self):
        self.assertEqual(run("""
            dec choose[a,b]: () -> sint;
            imp{x,y} choose[x,y] = lam(u: ()) => 1;
            imp{x} choose[x,x] = lam(u: ()) => 2;
            (@choose[sint,bool](), @choose[sint,sint]())
        """), (1, 2))
        self.assertEqual(run("""
            dec choose[a]: () -> sint;
            imp{x} choose[x] = lam(u: ()) => 1;
            imp choose[sint] = lam(u: ()) => 2;
            (@choose[sint](), @choose[bool]())
        """), (2, 1))
        # Latest matching implementation wins, not "most specific".
        self.assertEqual(run("""
            dec choose[a]: () -> sint;
            imp choose[sint] = lam(u: ()) => 2;
            imp{x} choose[x] = lam(u: ()) => 1;
            @choose[sint]()
        """), 1)

    def test_compound_patterns_and_static_substitution(self):
        self.assertEqual(run("""
            dec id[t]: t -> t;
            imp{a,b} id[(a,b)] = lam(pair: (a,b)) => pair;
            @id[(sint,bool)](7, true)
        """), (7, True))
        self.assertEqual(run("""
            dec choose[a]: () -> sint;
            imp choose[sint] = lam(u: ()) => 7;
            imp choose[bool] = lam(u: ()) => 9;
            dec outer[a]: () -> sint;
            imp{a} outer[a] =
              let
                dec inner[b]: () -> sint;
                imp{b} inner[b] = lam(u: ()) => @choose[a]();
                dec shadow[a]: () -> sint;
                imp{a} shadow[a] = lam(u: ()) => @choose[a]()
              in lam(u: ()) => @inner[bool]() + @shadow[bool]() end;
            @outer[sint]()
        """), 16)

    def test_local_exports_capture_private_environments(self):
        self.assertEqual(run("""
            dec helper: sint;
            imp helper = 1;
            local
                val hidden = 40;
                imp helper = 2
            in
                val f = lam(u: ()) => hidden + @helper;
                dec public: () -> sint;
                imp public = f
            end;
            (@public(), @helper)
        """), (42, 1))
        d, t = libfile_evaluate(libfile_parse("""
            local val secret = 1; imp private = 2
            in val public = lam(u: ()) => secret + @private end
        """))
        self.assertNotIn("secret", d)
        self.assertFalse(t)
        self.assertEqual(d2exp_evaluate(d2exp_parse("public()"), d, t), 3)

    def test_zero_argument_templates_and_alias(self):
        self.assertEqual(run("dec x: sint; imp x = 42; (@x, @x[])"), (42, 42))
        self.assertEqual(run("""
            dec x[a]: () -> sint;
            imp x[sint] = lam(u: ()) => 42;
            @x[int]()
        """), 42)

    def test_runtime_errors_without_typechecking(self):
        for source in (
            "unknown", "@missing", "1(2)", "if 1 then 2 else 3",
            "true + 1", "-false", "true == 1", "(1,) == (1,)",
            "1/0", "1%0", "1.0", "(1,).2",
            "(lam(x: sint) => x)[sint]",
        ):
            with self.subTest(source=source), self.assertRaises(FWATS3RuntimeError):
                d2exp_evaluate(d2exp_parse(source))
        d, t = libfile_evaluate(libfile_parse(
            "imp{x} same[x,x] = 1;"))
        with self.assertRaises(FWATS3RuntimeError):
            d2exp_evaluate(d2exp_parse("@same[sint,bool]"), d, t)
        with self.assertRaises(FWATS3RuntimeError):
            d2exp_evaluate(d2exp_parse("@same[a,a]", static_vars={"a"}), d, t)

    def test_declarations_do_not_execute_implementation_bodies(self):
        d, t = libfile_evaluate(libfile_parse("imp bad = 1/0;"))
        with self.assertRaises(FWATS3RuntimeError):
            d2exp_evaluate(d2exp_parse("@bad"), d, t)

    def test_ast_is_not_modified_by_instantiation(self):
        tree = libfile_parse("imp{a} id[a] = lam(x: a) => x;")
        before = repr(tree)
        d, t = libfile_evaluate(tree)
        self.assertEqual(d2exp_evaluate(d2exp_parse("@id[sint](1)"), d, t), 1)
        self.assertEqual(d2exp_evaluate(d2exp_parse("@id[bool](true)"), d, t), True)
        self.assertEqual(repr(tree), before)


if __name__ == "__main__":
    unittest.main()
