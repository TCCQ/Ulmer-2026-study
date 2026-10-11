# FWATS3 level-2 syntax and parser

Requires Python 3.12 or later. No third-party dependencies or build step are
needed. Run commands from this directory.

## Modules

- `FWATS3_2basics.py` defines the level-2 AST, functional lists/options, and
  static matching contexts.
- `FWATS3_2staexp.py` provides structural equality and directional type matching.
- `FWATS3_2interp.py` evaluates code with separate variable/template environments.
- `FWATS3_2tcheck.py` checks expression types and template declarations/implementations.
- `FWATS3_2parser.py` parses the concrete syntax described in `SYNTAX.md`.
- `mylib/1.fwats3` contains sample constant declarations and implementations.

## Parse source

Entry points accept source text, not file paths. All consume their entire
input and return existing level-2 AST nodes.

```python
from pathlib import Path
from FWATS3_2parser import (
    libfile_parse, program_parse, d2exp_parse, s2exp_parse,
)

path = Path("mylib/1.fwats3")
library = libfile_parse(path.read_text(), filename=str(path))

program = program_parse("""
dec identity[a]: a -> a;
imp{a} identity[a] = lam(x: a) => x;
@identity[int](42)
""")

expression = d2exp_parse("lam(x: int) => x + 1")
signature = s2exp_parse("a -> list[a]", static_vars={"a"})
```

- `libfile_parse` (also named `d2eclist_parse`) returns `d2eclist`.
- `program_parse` returns `D2Elets` with a present, required final expression.
- `d2exp_parse` returns `d2exp`.
- `s2exp_parse` returns `s2exp`.

All entry points accept an optional `filename` for error reporting.
Expression and type entry points also accept `static_vars`, an iterable of
in-scope static variable names. Otherwise, static names are constructors.
Declaration parameters establish their own scopes automatically.

Invalid syntax raises `FWATS3SyntaxError`, a `SyntaxError` subclass with
`filename`, `lineno`, `offset`, and `text` attributes. Lines and columns
are one-based. Parsing does not evaluate expressions or type-check programs.
The parser rejects duplicate static parameters, application of a bound static
variable, and identical fix/parameter names. Checking unused implementation
parameters, constructor arities, signature compatibility, and implementation
selection are separate from parsing. See the type-checker rules below.

Declarations require separating semicolons. Use a semicolon before a program's
final expression to avoid its being consumed as part of the last initializer.
Recursive descent is subject to Python's recursion limit for deeply nested input.

## Type checking

```python
from pathlib import Path
from FWATS3_2parser import libfile_parse, d2exp_parse, program_parse
from FWATS3_2tcheck import libfile_tcheck, d2exp_tcheck, program_tcheck

env = libfile_tcheck(libfile_parse(Path("mylib/1.fwats3").read_text()))
result_type = d2exp_tcheck(d2exp_parse("@f1_fact[sint](5)"), env)
# S2Econ("sint", fnlist_nil())

result_type = program_tcheck(program_parse("val x = 6; x * 7"), env)
```

`d2exp_tcheck` returns an expression's static type. `program_tcheck` checks
a parsed program with a required final expression. `libfile_tcheck` (also
`d2eclist_tcheck`) returns a `TypeEnvironment` holding variable types, template
signatures, and checked implementation records. Supplying an environment lets
later files use earlier declarations. Checking does not mutate the supplied
environment or AST. Errors raise `FWATS3TypeError`; declaration errors name
their declaration. Source locations are unavailable because the AST does not
store them.

The implemented rules are:

- Integer literals have type `sint`; `int` is normalized to `sint` for the
  earlier examples. Boolean and string literals have types `bool` and `string`.
  Unit has type `()`.
- Arithmetic, modulo, unary signs, and ordering comparisons require `sint`.
  Equality/inequality accept two operands of the same primitive type: `sint`,
  `bool`, or `string`. Functions and generic type variables do not have
  primitive equality.
- Function arguments, annotated fix results, and conditional branches must
  agree structurally. Conditions must be boolean; projections require an
  in-bounds tuple index. A bodyless let has unit type.
- `dec` records a polymorphic signature. `imp` requires a prior signature,
  substitutes its static patterns for the signature parameters, and checks
  the implementation against that resulting type. Its own parameters are
  rigid, distinct type variables and must all occur in its patterns.
- Template references require the declared number of explicit static arguments.
  A zero-parameter constant can be referenced with or without `[]`.
  Static application currently requires a `D2Ecst`; templates are not
  first-class polymorphic values.
- Declarations are sequential; variables may shadow previous bindings.
  Duplicate visible constant signatures are rejected. Local declarations
  expose only the public region; exported functions can capture private names.
  Implementations of private signatures remain private.
- Known constructors have checked arities (`sint`, `bool`, `string`: zero;
  `list`, `optn`: one). Other constructors remain nominal and accept any
  arity because the AST has no type-constructor declarations.

Signature checking does not choose an implementation, require an implementation
to exist at each call, or reject overlapping implementations. Dispatch,
evaluation, termination, and runtime arithmetic errors remain separate concerns.
Recursive checking is subject to Python's recursion limit.

## Interpretation

```python
from pathlib import Path
from FWATS3_2parser import libfile_parse, d2exp_parse
from FWATS3_2tcheck import libfile_tcheck, d2exp_tcheck
from FWATS3_2interp import libfile_evaluate, d2exp_evaluate

library = libfile_parse(Path("mylib/1.fwats3").read_text())
types = libfile_tcheck(library)
denv, tenv = libfile_evaluate(library)

expression = d2exp_parse("@f1_fact[sint](5)")
d2exp_tcheck(expression, types)
value = d2exp_evaluate(expression, denv, tenv)  # 120
value = d2exp_evaluate(d2exp_parse("@f1_fibo[sint](10)"), denv, tenv)  # 55
```

`libfile_evaluate` (also `d2eclist_evaluate`) returns new `(denv, tenv)`.
`d2exp_evaluate` and `program_evaluate` return a runtime value. All accept
optional existing environments. Values are Python integers, booleans, strings,
tuples (including unit `()`), or `D2Vlam`/`D2Vfix` closure records.

The two environments have distinct roles:

- `denv` is a read-only mapping from dynamic variable names to values.
- `tenv` is a newest-first tuple of `TemplateClosure` records.
- Each function closure stores its expression and both definition-time
  environments. Each template closure stores its `D2Cimpl` and both
  definition-time environments.
- A template reference searches the current `tenv` for the most recent
  matching implementation. `s2exp_match` compares tuple wrappers around the
  entire pattern/argument lists, so repeated variables share one match context.
  There is no additional specificity ranking.
- The resulting static bindings are substituted into a fresh implementation
  body, respecting nested `dec`/`imp` binders. Evaluation then uses that
  implementation's captured `denv` and `tenv`. Requested static arguments
  must be ground; `int` is normalized to `sint`.
- Implementations are stored without evaluating their bodies. Each reference
  instantiates and evaluates its selected body anew; there is no memoization.
  An implementation captures the template environment preceding its own
  insertion. Use `fix` for recursion, as in factorial and Fibonacci.
- `dec` is erased at runtime. `local` evaluates the private region, then the
  public region, and exports only public variable bindings and implementation
  closures. Captured environments retain private dependencies.

Evaluation is eager, left to right; conditionals evaluate only the selected
branch. Integer division uses Python floor division, and modulo uses Python
remainder semantics. Runtime checks reject invalid operand types, non-functions,
invalid projections, unbound names, and missing implementations with
`FWATS3RuntimeError`. Division/modulo by zero also raises that exception.

Type checking is explicit and separate from evaluation; run it first for source
validation. The interpreter does not infer static arguments or perform
higher-order template application. It uses Python recursion without tail-call
optimization, so deep recursion is subject to Python's recursion limit.

## Tests

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -v
git diff --check
```

Tests cover AST construction, precedence, static scopes, tuples, template
patterns, declarations, program/library boundaries, errors, and the sample
library. New Python code uses four-space indentation and standard-library
`unittest`.
