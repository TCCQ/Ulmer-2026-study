# Level-2 Python implementation

`FWATS3_2basics.py` defines the syntax and linked-list/option types.
`FWATS3_2staexp.py` provides `s2exp_equal(left, right)` for structural
type equality: constructor kinds and names must match, variables compare by
name, and function and tuple components and constructor arguments compare
recursively in order. Argument counts must match. Two bare `S2E000()`
placeholders compare equal; a placeholder does not equal a concrete type.
Run its tests with `make -C TEST staexp`.
`s2exp_match(pattern, target)` binds variables in the pattern to target
types, returning `fnoptn_cons(context)` on success (including `CTXnil()`
when no bindings are needed) or `fnoptn_nil()` on failure. Repeated pattern
variables require structurally equal target types. Target variables remain
literal; matching does not perform unification. Each `CTXcns` contains a
name, its matched type, and the remaining context. New bindings are prepended
during left-to-right traversal, and each call starts with an empty context.
`FWATS3_2interp.py` provides `d2exp_evaluate` for integer, boolean, and string
literals, integer operators, conditionals, tuples, let expressions, closures,
and recursive functions.
String literals (`D2Estr`) evaluate to string values (`D2Vstr`) and can be
passed to and returned from functions. Template resolution is not implemented
by this interpreter.

Tuple expressions (`D2Etupl`) evaluate their elements from left to right in
the current environment and return `D2Vtupl`. Its `arg1` holds a linked list
of values (`d2valist`), preserving element order and nested tuples. Empty
tuples are represented by `D2Vtupl(fnlist_nil())`. The unit constructor
`D2Vnil()` is a function returning this empty tuple:

```python
def D2Vnil() -> D2Vtupl:
    return D2Vtupl(fnlist_nil())
```

`D2Eproj(index, expression)` selects a tuple element using a zero-based index.
Negative or out-of-range indices raise `IndexError`; projecting from a
non-tuple value raises `TypeError`.

`d2ecl_evaluate(decl, denv)` returns an extended environment for `D2Cbind`
and `D2Clocal`, leaving the input environment unchanged. Binding expressions
use the incoming environment. Local declarations export only their public
bindings; exported closures retain access to private bindings.
`d2eclist_evaluate(decls, denv)` evaluates a declaration list in order.
Unsupported declarations, including `D2Cimpl`, raise `TypeError`.

`D2Elets` evaluates its declarations in order, then evaluates its optional
body in the extended environment. An absent body returns the unit value
`D2Vnil()`, equal to the result of evaluating an empty `D2Etupl`.
Let bindings stay within that scope, and returned closures retain access to
them.

Use Python 3.12 and a version of `mypy` that supports Python 3.12 type syntax
(verified with mypy 2.1.0). Runtime code and tests require only the Python
standard library.
Type declarations use Python 3.12 `type` aliases and generic class syntax.
Match the surrounding Python style and use four spaces for indentation.

Run from this directory:

```sh
make -C TEST
```

The default target checks all three modules and their tests with
`mypy --strict`, then runs the interpreter and structural equality tests.
To run interpreter tests or type checking separately:

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
Tuple tests cover empty tuples, evaluated elements, nested tuples, and the
equivalence of `D2Vnil()` and an empty tuple value.

`TEST/test01_2interp.py` implements tail-recursive factorial with a curried
accumulator: `loop(n)(acc)` returns `acc` when `n == 0`, otherwise it calls
`loop(n - 1)(n * acc)`. Tests compare `loop(n)(1)` against Python's
`math.factorial` and check a non-unit initial accumulator. The interpreter
uses the Python call stack and does not perform tail-call optimization.

The same test file implements Fibonacci with tuple state `(n, a, b)`.
Starting from `(n, 0, 1)`, each recursive step constructs `(n - 1, b, a + b)`
using tuple projections; when `n` reaches zero, the function returns `a`.
Tests include `F(0) = 0`, `F(1) = 1`, and `F(20) = 6765`.
A `D2Elets` example binds `n = 10`, the Fibonacci function, and the initial
tuple `(n, 0, 1)` in sequence, then calls the bound function in the let body
and checks that the result is `55`.

The Makefile defaults to `python3` and `mypy` on `PATH`. Override `PYTHON`
or `MYPY` on the make command line to select another installation.
