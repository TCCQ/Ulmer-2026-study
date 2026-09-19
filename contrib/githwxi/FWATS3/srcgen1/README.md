# Level-2 Python implementation

`FWATS3_2basics.py` defines the syntax and linked-list/option types.
`FWATS3_2interp.py` provides `d2exp_evaluate` for integer, boolean, and string
literals, integer operators, conditionals, closures, and recursive functions.
String literals (`D2Estr`) evaluate to string values (`D2Vstr`) and can be
passed to and returned from functions. Template resolution is not implemented
by this interpreter.

Use Python 3.12 and a version of `mypy` that supports Python 3.12 type syntax
(verified with mypy 2.1.0). Runtime code and tests require only the Python standard library.
Type declarations use Python 3.12 `type` aliases and generic class syntax.
Match the surrounding Python style and use four spaces for indentation.

Run from this directory:

```sh
make -C TEST
```

The default target checks both modules and `TEST/test00_2interp.py` with
`mypy --strict`, then runs that test file. To run either check separately:

```sh
make -C TEST interp
make -C TEST tcheck
```

The tests cover captured environments, recursive factorial, string literal
contents (including empty and Unicode strings), and string arguments passed
through a lambda.

The Makefile defaults to `python3` and `mypy` on `PATH`. Override `PYTHON`
or `MYPY` on the make command line to select another installation.
