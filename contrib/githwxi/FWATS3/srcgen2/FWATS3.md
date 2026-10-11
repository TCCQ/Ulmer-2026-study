# FWATS3: Syntax, Type Checking, and Interpretation

This document describes the current Python implementation of FWATS3. Its main
feature is a separation between ordinary variables and template constants:
variables have lexical value bindings, while template constants have declared
types and separately registered implementations selected by static arguments.
Functions and template implementations both capture their definition-time
variable and template environments.

The implementation requires Python 3.12 or later and only the standard library.
[SYNTAX.md](SYNTAX.md) gives the full concrete grammar;
[README.md](README.md) gives entry points and commands. This document explains
how the syntax, checker, and interpreter fit together.

## 1. Implementation structure

| Module | Responsibility |
| --- | --- |
| [FWATS3_2basics.py](FWATS3_2basics.py) | Static types, expressions, declarations, functional lists/options, matching contexts |
| [FWATS3_2parser.py](FWATS3_2parser.py) | Tokenization and recursive-descent parsing into level-2 trees |
| [FWATS3_2staexp.py](FWATS3_2staexp.py) | Structural equality and directional static-type matching |
| [FWATS3_2tcheck.py](FWATS3_2tcheck.py) | Expression typing, signature instantiation, implementation checking |
| [FWATS3_2interp.py](FWATS3_2interp.py) | Evaluation with two lexical environments and template dispatch |
| [mylib/1.fwats3](mylib/1.fwats3) | Numeric template constants, generic factorial, and generic Fibonacci |

The execution path is explicit:

```text
source text --parser--> level-2 AST --type checker--> static type/environment
                                  \--interpreter--> value/runtime environments
```

The checker does not rewrite the AST into a compiled program. The interpreter
accepts the same AST and does not automatically invoke the checker. A normal
client parses, checks, then evaluates. Checking verifies types but does not
prove that a matching template implementation will exist at runtime.

AST sequences use `fnlist_nil` and `fnlist_cons`; optional values use
`fnoptn_nil` and `fnoptn_cons`. Python lists in conceptual examples below are
abbreviations for these functional lists.

## 2. Concrete syntax

### 2.1 Files, names, and literals

A library file is a sequence of declarations. A program is a sequence of
declarations followed by one required result expression. The declaration
sequence may be empty in either case.

```text
libfile = declarations
program = declarations expression
```

Semicolons separate adjacent declarations; the final declaration may also have
a semicolon. A semicolon before the result expression avoids its being consumed
as part of the final initializer. There is no semicolon after a program's
result expression.

Identifiers match `[A-Za-z_][A-Za-z_0-9$]*`: `value$1` is valid, while `$value`
and `1_value` are not. Keywords are reserved and case-sensitive. Whitespace
and newlines are insignificant outside strings, and `#` begins a line comment.
Strings use double quotes and the escapes `\"`, `\\`, `\n`, `\r`, and `\t`.

Ordinary expression names denote variables. The `@` prefix denotes a template
constant reference:

```text
x                  # D2Evar("x")
@identity          # D2Ecst("identity")
@identity[sint]    # D2Etapp(D2Ecst("identity"), [sint])
```

Declaration and implementation headers use bare names, without `@`.

### 2.2 Static types

Types have four concrete AST forms:

| Syntax | Static tree | Meaning |
| --- | --- | --- |
| `a`, when statically bound | `S2Evar("a")` | Type variable |
| `sint`, `list[a]` | `S2Econ(name, arguments)` | Named type constructor |
| `a -> b` | `S2Efun(a, b)` | Function type |
| `()`, `(a,)`, `(a,b)` | `S2Etupl(components)` | Tuple type |

A name is a type variable when bound by an enclosing static parameter list;
otherwise it is a constructor. There is no apostrophe prefix for variables.
`(a)` means `a`, while `(a,)` is a singleton tuple. Arrows associate to the
right: `a -> b -> c` means `a -> (b -> c)`.

The checker uses `sint`, `bool`, and `string` as primitive types. It normalizes
`int` to `sint` for compatibility with earlier examples. Unit is the empty
tuple type `()`. The known constructors `list` and `optn` each take one argument;
they do not by themselves supply runtime list or option operations. Other
constructor names are nominal and have no checked arity in this version.

### 2.3 Functions, application, and expressions

A lambda requires a parameter type. A recursive function also requires a result
type and a distinct recursive name:

```fwats3
val increment = lam(x: sint) => x + 1;
val fact = fix loop(n: sint): sint =>
  if n <= 1 then 1 else n * loop(n - 1);
fact(increment(4))
```

This program returns `120`. Every function takes one dynamic argument.
`f(x,y)` passes one pair; `f()` passes unit; `f(x,)` passes a singleton tuple.
Curried calls use `f(x)(y)`. Projection `pair.0` selects the first component.

From strongest to weakest, expression precedence is:

| Level | Forms | Association |
| --- | --- | --- |
| Postfix | Calls, projections, static application | Left |
| Unary | `+`, `-` | Recursive prefix |
| Multiplicative | `*`, `/`, `%` | Left |
| Additive | `+`, `-` | Left |
| Comparison | `<`, `>`, `<=`, `>=`, `==`, `!=` | No chaining |

Lambda, fix, if, and let bodies extend as far right as their enclosing syntax
permits. Parenthesize them when used as arithmetic operands or the function in
a call. A conditional requires both branches.

The parser constructs `D2Elam`, `D2Efix`, `D2Eapp`, `D2Eif0`, `D2Etupl`, and
`D2Eproj` directly. Unary signs use `D2Eop1`; binary operations use `D2Eop2`
with their literal operator spelling. No expression is evaluated by parsing.

### 2.4 Let and local declarations

`val` bindings are sequential: a binding is available to subsequent declarations
and the body, but not to its own initializer. A later variable binding may
shadow an earlier one.

```fwats3
let
  val x = 6;
  val y = 7
in
  x * y
end
```

This expression returns `42`. A let without `in`, such as `let val x = 6 end`,
processes its declarations and returns unit. In the AST these forms are
`D2Elets(declarations, fnoptn_cons(body))` and
`D2Elets(declarations, fnoptn_nil())`, respectively.

`local private_declarations in public_declarations end` is a declaration,
represented by `D2Clocal`. Private declarations are visible while processing
the public region. Only public declarations are exported. Exported closures
retain access to private bindings through their captured environments.

## 3. Template declarations, implementations, and uses

### 3.1 Declaring a signature

```fwats3
 dec identity[a]: a -> a;
 imp{a} identity[a] = lam(x: a) => x;
 @identity[sint](42)
```

This program returns `42`. The leading spaces are optional.

The `dec` declares a template constant and its type. Its bracketed identifiers
bind ordered static parameters over the signature after the colon. It maps to:

```text
D2Cdecl("identity", ["a"], S2Efun(S2Evar("a"), S2Evar("a")))
```

A declaration does not supply a value or an implementation. A signature may
have no static parameters, for example `dec answer: sint`. Such a constant
can be referenced as `@answer` or `@answer[]`.

### 3.2 Implementing a template constant

The implementation form separates bound parameters from argument patterns:

```text
imp{parameters} name[static_patterns] = expression
```

For identity, the AST is:

```text
D2Cimpl(
  "identity",
  D2Elam("x", S2Evar("a"), D2Evar("x")),
  ["a"],
  [S2Evar("a")]
)
```

Braces bind implementation parameters over the patterns and the expression,
including annotations and nested static applications. Brackets contain the
patterns to match against a use's static arguments. Either list may be omitted;
empty braces are invalid, while empty brackets are allowed.

Signature and implementation parameters are independently bound. An
implementation can rename them, specialize them, or place several parameters
inside a single pattern:

```fwats3
dec identity[t]: t -> t;
imp{a,b} identity[(a,b)] = lam(x: (a,b)) => x;
@identity[(sint,bool)](42, true)
```

The result is `(42, true)`. There are two implementation parameters, but only
one static pattern: `(a,b)`. The number of patterns must match the number of
signature parameters, not the number of implementation parameters.

An implementation need not be a function. For example, an implementation of
`dec zero[a]: a` can be a variable expression holding a value of the selected
type. In that case `@zero[sint]` produces a value directly, without a dynamic
call `()`.

### 3.3 Using a template constant

`@identity[sint](42)` has two separate AST operations:

```text
D2Eapp(
  D2Etapp(D2Ecst("identity"), [S2Econ("sint", [])]),
  D2Eint(42)
)
```

Static application instantiates the template and yields its value. Dynamic
application then calls that value with `42`. A specialized function value can
be stored in an ordinary variable and passed to another function. Unspecialized
templates are not first-class polymorphic values in this implementation.

The grammar permits static application after an arbitrary expression, but the
checker and interpreter currently require its target to be a `D2Ecst`.
Static arguments are explicit; there is no inference from dynamic arguments.

## 4. Type-checker implementation

### 4.1 Environments and ordinary expression rules

`TypeEnvironment` holds three dictionaries: variable types, template signatures,
and checked implementation records. Public checking calls return results or
extended environments without changing the caller's environment. A separate
static scope maps source parameter names to fresh internal type variables.

The checker synthesizes an expression's type from its syntax and annotations.
It compares normalized types using `s2exp_equal`, which checks constructor names,
ordered children, and variable identities structurally.

| Expression | Rule |
| --- | --- |
| Integer, boolean, string | `sint`, `bool`, `string` |
| Lambda | Bind its annotated argument; infer the body; produce a function type |
| Fix | Bind its annotated function type and parameter; check its body against the declared result |
| Application | Require a function and a structurally matching argument |
| Conditional | Require a boolean condition and equal branch types |
| Tuple/projection | Infer components; require a tuple and an in-bounds index |
| Let | Check declarations sequentially, then its body; use unit when bodyless |

Arithmetic, modulo, unary signs, and ordering comparisons require `sint`.
Equality and inequality accept matching primitive types among `sint`, `bool`,
and `string`. They do not supply generic equality for arbitrary type variables.

### 4.2 Checking dec and imp

For `dec name[p1,...,pn]: T`, the checker creates fresh identities for the
parameters, normalizes and validates `T`, then stores a `TemplateSignature`.
Duplicate visible constant declarations are rejected. Variable shadowing remains
allowed.

For `imp{q1,...,qm} name[P1,...,Pn] = e`, checking proceeds as follows:

1. Look up a previously declared signature for `name`.
2. Introduce fresh, rigid identities for the implementation's parameters.
3. Check its static patterns in that scope. Every implementation parameter must
   occur in at least one pattern.
4. Require one pattern per signature parameter.
5. Substitute the patterns for the signature parameters in the declared type.
6. Infer the body's type under the implementation scope and require structural
   equality with the substituted signature.
7. Record the checked implementation.

For `identity[t]: t -> t` and the pattern `(a,b)`, the required body type is
`(a,b) -> (a,b)`. For a concrete pattern `sint`, it is `sint -> sint`.
The implementation parameters themselves are rigid: the checker cannot silently
choose `sint` for a generic `a` to make primitive arithmetic type-check.

Fresh identities matter under nesting. If an inner implementation binds a new
`a`, it must not be confused with the outer `a`, even though both have the same
source spelling. The checker generates internal names such as `a#17`; these
are not source identifiers.

### 4.3 Checking template uses

The checker types `@name[T1,...,Tn]` by finding the signature and substituting
the supplied types into it. It does not search the implementations. It rejects
unknown constants, incorrect static arity, invalid constructor arity, and
unbound static variables in directly constructed ASTs.

Consequently, a declared use can type-check even if no implementation is
available. Implementation availability and selection are runtime operations.
Overlapping implementations are accepted if each independently satisfies the
signature.

This separation permits a generic factorial body to use `@f1_mul[a]`: its type
comes from `f1_mul`'s signature. Selection of multiplication for a concrete type
occurs only after factorial has been instantiated.

## 5. Two-environment interpreter

### 5.1 Values and closure representations

Runtime values are Python integers, booleans, strings, tuples, and explicit
function closures. Unit is the empty Python tuple. There are two environments:

| Environment | Representation | Purpose |
| --- | --- | --- |
| `denv` | Read-only mapping from names to values | Resolve `D2Evar` |
| `tenv` | Tuple of template closures, newest first | Resolve `D2Ecst` and `D2Etapp` |

The three closure records are:

```text
D2Vlam(expression, denv, tenv)
D2Vfix(expression, denv, tenv)
TemplateClosure(implementation, denv, tenv)
```

Both function forms and each stored implementation capture both environments.
Variable environments are copied into read-only mapping snapshots; template
environments are tuples of existing closure records. Subsequent declarations
do not change a previously captured environment. AST objects are treated as
unchanging by the interpreter, although the AST dataclasses themselves are not
frozen.

### 5.2 Evaluating ordinary code

Evaluation is eager and left to right. A call evaluates its function expression,
then its argument, then applies the resulting closure. Tuple components and
binary operands are also evaluated left to right. A conditional evaluates only
the selected branch.

Applying a lambda copies its captured `denv`, binds the parameter to the
argument, and evaluates the body using its captured `tenv`. Applying a fix
closure additionally binds the recursive name to the closure itself. Neither
operation substitutes the caller's environments for the captured ones.

Integer division uses floor division and `%` uses Python's remainder semantics;
for example, `-7 / 3` is `-3` and `-7 % 3` is `2`. Boolean values are rejected
as integer operands despite Python's boolean/integer inheritance relationship.

### 5.3 Registering an implementation

Processing an `imp` does not evaluate its body. Instead, the interpreter builds
`TemplateClosure(implementation, current_denv, current_tenv)` and prepends it
to `tenv`. A `val` evaluates its initializer and extends `denv`; a `dec` has no
runtime effect.

The template environment captured by an implementation precedes its own
insertion. Recursive functions should therefore use `fix`. An implementation
body is not automatically recursive through its template constant name; such a
reference searches the captured environment and can find only older entries.

### 5.4 Matching and instantiation

To evaluate `@name[T1,...,Tn]`, the interpreter:

1. Normalizes static arguments, including the alias `int` to `sint`, and requires
   them to be ground: there must be no remaining `S2Evar` nodes.
2. Scans the current `tenv` from newest to oldest, considering entries named
   `name`.
3. Wraps the implementation's pattern list and the requested argument list in
   separate `S2Etupl` nodes and calls `s2exp_match(pattern_tuple, argument_tuple)`.
4. Continues after a failed match. On success, converts the returned `s2ctx`
   into a substitution and checks that its bound names agree with the
   implementation's declared parameters.
5. Substitutes those types into a fresh copy of the implementation body.
6. Evaluates that body using the selected implementation closure's captured
   `denv` and `tenv`.

The first successful candidate wins. Selection uses declaration order, with no
additional preference for concrete or structurally more specific patterns.
Each reference evaluates a freshly instantiated body; results are not cached.

`s2exp_match` is directional matching, not symmetric unification. Variables
on the left bind to types on the right. Repeated left variables must match
structurally equal types. Constructors must agree in name and arity, and
function and tuple components match in order.

Wrapping the whole argument list in a tuple gives all patterns one shared
matching context. For a pattern list `[a,a]`, requests `[sint,sint]` succeed,
while `[sint,bool]` fail. Matching each argument with a separate fresh context
would incorrectly accept the second request.

The same machinery supports `[(a,b)]`, `[list[a]]`, and function-type patterns.
The wrapper around a one-element pattern list is distinct from a tuple type
inside that list.

### 5.5 Static substitution and nested scopes

Substitution visits annotations, static applications, and nested declarations
throughout the body. It leaves dynamic variable names unchanged. On entering
a nested `dec` or `imp`, bindings for that declaration's own static parameters
are removed from the substitution so that inner binders shadow outer names.
Other outer bindings continue into the nested declaration.

Because runtime matches bind variables to ground types, substitutions introduce
no free type variables that could be captured by an inner binder. The source
implementation tree is not modified, so later instantiations can choose other
types independently.

The resulting body can create a function closure. That closure captures the
selected implementation's environments, possibly extended by a let inside the
implementation. This preserves lexical scope across both instantiation and
subsequent dynamic function calls.

## 6. Worked template examples

### 6.1 A private value captured by an implementation

```fwats3
dec zero[a]: a;
local
  val zi = 0
in
  imp zero[sint] = zi
end;
val zi = 1;
@zero[sint]
```

The program has type `sint` and returns `0`.

When the implementation is registered, its `denv` contains the private
`zi = 0`. Leaving `local` exports the implementation closure but does not export
the private variable. The later `val zi = 1` creates an outer binding.
The final template use selects `zero[sint]`, whose body `zi` is evaluated in
the captured environment containing zero.

### 6.2 Capturing template dependencies

```fwats3
dec helper: sint;
imp helper = 10;
dec saved: sint;
imp saved = @helper;
imp helper = 20;
(@saved, @helper)
```

The result is `(10,20)`. `saved` captures the template environment containing
the first helper implementation. Its body continues to select that helper,
while the final direct reference sees the newer implementation.

Selection therefore has two distinct moments: the caller's current `tenv`
selects an implementation, then that implementation's captured `tenv` governs
any template references inside its body.

### 6.3 Overlapping patterns

```fwats3
dec choose[a,b]: () -> sint;
imp{x,y} choose[x,y] = lam(u: ()) => 1;
imp{x} choose[x,x] = lam(u: ()) => 2;
(@choose[sint,bool](), @choose[sint,sint]())
```

The result is `(1,2)`. The newer repeated-variable pattern rejects unequal
arguments and accepts equal ones. Reversing the implementation declarations
would make the general pattern win both calls, yielding `(1,1)`.

### 6.4 Generic factorial and Fibonacci

The library declares template constants for constructing numeric values,
comparison, and arithmetic. Generic factorial can consequently avoid primitive
arithmetic on a rigid type variable:

```fwats3
dec f1_fact[a]: a -> a;
imp{a} f1_fact[a] =
let
  val one = @f0_1[a]()
in
  fix fact(n: a): a =>
    if @f1_lte[a](n, one) then one
    else @f1_mul[a](n, fact(@f1_sub[a](n, one)))
end;
```

This is a library fragment requiring the previously declared and implemented
numeric template constants in `mylib/1.fwats3`.

For `@f1_fact[sint](5)`, matching binds `a` to `sint`. Substitution specializes
all annotations and nested template uses. Evaluating the let obtains integer
one; the fix closure captures that variable and factorial's definition-time
template environment. Recursive calls use the `fact` variable in `denv`.
Arithmetic and comparisons use `tenv`. The result is `120`.

Fibonacci follows the same pattern, constructing one with `@f0_1[a]()` and two
with `@f1_si[a](2)`. It returns its argument for inputs at most one; otherwise
it adds the two recursive results through `@f1_add[a]`. With the supplied
`sint` implementations, `@f1_fibo[sint](10)` returns `55`.

These functions are intended for nonnegative integer-like inputs. Their types
do not prove that implementations obey arithmetic laws or that recursion
terminates. Fibonacci uses direct recursive evaluation without memoization.

## 7. Running the implementation

From this directory:

```python
from pathlib import Path
from FWATS3_2parser import libfile_parse, d2exp_parse
from FWATS3_2tcheck import libfile_tcheck, d2exp_tcheck
from FWATS3_2interp import libfile_evaluate, d2exp_evaluate

path = Path("mylib/1.fwats3")
library = libfile_parse(path.read_text(), filename=str(path))
types = libfile_tcheck(library)
denv, tenv = libfile_evaluate(library)

expression = d2exp_parse("@f1_fact[sint](5)")
result_type = d2exp_tcheck(expression, types)
result = d2exp_evaluate(expression, denv, tenv)
assert result == 120
```

For a complete program, use `program_parse`, `program_tcheck`, and
`program_evaluate`. Declaration-processing functions return extended
environments so multiple library files can be processed sequentially.

| Stage | Failure type | Information |
| --- | --- | --- |
| Parsing | `FWATS3SyntaxError` | Filename, line, column, source line |
| Type checking | `FWATS3TypeError` | Type mismatch or invalid declaration; declaration context where available |
| Interpretation | `FWATS3RuntimeError` | Missing binding/implementation or invalid runtime operation |

The checker and interpreter cannot report source locations because level-2 AST
nodes do not retain them. Division/modulo by zero is reported as a runtime
error. Python's recursion limit still applies to deeply nested syntax and
recursive evaluation; there is no tail-call optimization.

## 8. Current boundaries and verification

The implementation has no implicit static-argument inference, no higher-order
polymorphic template values, and no automatic specialization priority. Type
checking does not establish implementation availability, runtime safety from
division by zero, or termination. General type-constructor declarations and
runtime list/option libraries are not implemented.

At runtime, `dec` is erased, so lexical signature visibility is enforced by the
checker rather than by `tenv`. In particular, the checker keeps private
signatures inaccessible outside `local`; the interpreter tracks implementation
closures and exports those created in the public region. Evaluate checked
programs when relying on signature visibility rules.

The test suites are `tests/test_parser.py`, `tests/test_tcheck.py`, and
`tests/test_interp.py`. They cover parsing, type errors, generic implementations,
static scope, closure capture of both environments, compound/repeated patterns,
overlap order, private dependencies, and execution of the numeric library.

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -v
git diff --check
```
