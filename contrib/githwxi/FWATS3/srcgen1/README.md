# Level-2 Python implementation

`FWATS3_2basics.py` defines the syntax and linked-list/option types.
`FWATS3_2interp.py` provides `d2exp_evaluate` for integer, boolean, and string
literals, integer operators, conditionals, closures, and recursive functions.
String literals (`D2Estr`) evaluate to string values (`D2Vstr`) and can be
passed to and returned from functions. Template resolution is not implemented
by this interpreter.

Tuple expressions (`D2Etupl`) evaluate their elements from left to right in
the current environment and return `D2Vtupl`. Its `arg1` holds a linked list
of values (`d2valist`), preserving element order and nested tuples. Empty
tuples are represented by `D2Vtupl(fnlist_nil())`; `D2Vnil()` constructs this
same empty tuple as the unit value.

`d2ecl_evaluate(decl, denv)` returns an extended environment for `D2Cbind`
and `D2Clocal`, leaving the input environment unchanged. Binding expressions
use the incoming environment. Local declarations export only their public
bindings; exported closures retain access to private bindings.
`d2eclist_evaluate(decls, denv)` evaluates a declaration list in order.
Unsupported declarations, including `D2Cimpl`, raise `TypeError`.

`D2Elets` evaluates its declarations in order, then evaluates its optional
body in the extended environment. An absent body returns the unit value
`D2Vnil()`. Let bindings stay within that scope, and returned closures retain
access to them.

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
through a lambda. Declaration tests cover shadowing, sequential bindings,
local visibility, captured private bindings, and unsupported declarations.
Let-expression tests cover sequential bindings, scope, returned closures,
and declaration evaluation when the body is absent.
Tuple tests cover empty tuples, evaluated elements, and nested tuples.

The Makefile defaults to `python3` and `mypy` on `PATH`. Override `PYTHON`
or `MYPY` on the make command line to select another installation.
