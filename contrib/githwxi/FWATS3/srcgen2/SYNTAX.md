# FWATS3 concrete syntax design

This document defines the concrete syntax parsed by `FWATS3_2parser.py` into
the level-2 trees in `FWATS3_2basics.py`. `FWATS3_2tcheck.py` implements
static checking, and `FWATS3_2interp.py` evaluates code using separate variable
and template environments. See `README.md` for their rules and APIs.
`FWATS3_2staexp.py` provides structural static-expression equality and
directional matching. See `README.md` for parser entry points and test commands.

The design keeps single-argument functions and tuple arguments from the original
LAMBDA syntax, and adds explicit static types, named constants, template
applications, constant signature declarations, implementation declarations, and
local declarations.

## Lexical conventions

Identifiers match `[A-Za-z_][A-Za-z_0-9$]*`. The first character must be a
letter or underscore; subsequent characters may also include digits and `$`.
For example, `foo$bar`, `x$1`, and `name$` are valid, while `$foo` is invalid.
This rule applies to all identifier names, including static parameters and
constant names. Keywords are case-sensitive and reserved: `lam`, `fix`, `if`, `then`, `else`, `let`, `in`, `end`, `val`,
`dec`, `imp`, `local`, `true`, `false`, `D2CON`, `D2TCK`, and `D2ARG`.

Whitespace and newlines separate tokens but otherwise have no meaning. `#`
starts a comment through the end of the line, except inside a string.
Multi-character punctuation is recognized before its single-character prefixes.

Integer tokens are nonnegative decimal integers. A negative expression uses
unary minus. Strings use double quotes and support `\"`, `\\`, `\n`, `\r`, and
`\t`; unknown escapes and unescaped newlines are syntax errors. String literals
store their decoded contents in the AST.

Names use ordinary identifiers, with `@` marking dynamic constant references:

| Form | Meaning | Example |
| --- | --- | --- |
| `x` | Dynamic variable in expressions | `x`, `value` |
| `a` | Static variable when bound by a static parameter list; otherwise a type constructor | `a`, `int` |
| `@name` | Dynamic constant | `@print`, `@length` |

The `@` marker is not part of the stored name and must precede a non-keyword
identifier. Dynamic name references remain explicit: `f` means
`D2Evar("f")`, while `@f` means `D2Ecst("f")`. Static names are resolved
using their enclosing static parameter scopes.

## Grammar

In this EBNF, `{ ... }` means repetition, `[ ... ]` means optional, and quoted
text denotes literal tokens. `identifier`, `integer`, and `string` are lexical
tokens as defined above. The parser exposes separate entry points for one
complete expression, one complete static type, a library file, and a program;
each entry point must consume its entire input. A library file is a list of
declarations. A program is a list of declarations followed by one required
expression.

```text
libfile          = declarations ;
program          = declarations expression ;
declarations     = [ declaration { ";" declaration } [";"] ] ;
declaration      = "val" identifier "=" expression
                 | "dec" identifier [signature-params] ":" type
                 | "imp" [static-params] identifier [static-args] "=" expression
                 | "local" declarations "in" declarations "end" ;
signature-params = "[" [ identifier { "," identifier } [","] ] "]" ;
constant         = "@" identifier ;
static-params    = "{" identifier { "," identifier } [","] "}" ;
static-args      = "[" [ type { "," type } [","] ] "]" ;

expression       = "lam" "(" identifier ":" type ")" "=>" expression
                 | "fix" identifier "(" identifier ":" type ")"
                   ":" type "=>" expression
                 | "if" expression "then" expression "else" expression
                 | "let" declarations [ "in" expression ] "end"
                 | comparison ;
comparison       = additive [ comparison-op additive ] ;
comparison-op    = "<" | ">" | "<=" | ">=" | "==" | "!=" ;
additive         = multiplicative { ("+" | "-") multiplicative } ;
multiplicative   = unary { ("*" | "/" | "%") unary } ;
unary            = ("+" | "-") unary | postfix ;
postfix          = atom { parenthesized | "." integer | static-args } ;
atom             = integer | string | "true" | "false"
                 | generic-operator
                 | identifier | constant | parenthesized ;
generic-operator = ("D2CON" "(" string { "," expression } [","] ")"
                 | "D2TCK" "(" expression "," string ")"
                 | "D2ARG" "(" expression "," expression ")") ;
parenthesized    = "(" [ expression
                   [ "," [ expression { "," expression } [","] ] ] ] ")" ;

type             = type-atom [ "->" type ] ;
type-atom        = identifier [constructor-args]
                 | parenthesized-type ;
constructor-args = static-args
                 | "(" [ type { "," type } [","] ] ")" ;
parenthesized-type = "(" [ type
                     [ "," [ type { "," type } [","] ] ] ] ")" ;
```

Semicolons are required between adjacent declarations, with an optional final
semicolon. Empty declaration sequences are valid. No separator is needed before
`in` or `end`. Bare juxtaposition is not application.

A library file may contain zero declarations. A program may also have zero
declarations, but must end with exactly one expression. The optional final
semicolon in its declaration list may separate the last declaration from that
expression; use it explicitly for clarity. For example:

```text
# Library file: declarations only.
dec identity[a]: a -> a;
imp{a} identity[a] = lam (x: a) => x;
```

```text
# Program: declarations followed by the result expression.
dec identity[a]: a -> a;
imp{a} identity[a] = lam (x: a) => x;
@identity[int](42)
```

A program containing only `42` is also valid. An empty file is a valid library
file but not a valid program. There is no trailing semicolon after the program's
final expression. A library file translates to `d2eclist`; a program can be
represented by `D2Elets(declarations, fnoptn_cons(expression))`.

## Static types

Static syntax maps to the four concrete static-expression variants. A name
bound by a `dec` parameter list or an `imp` parameter list denotes `S2Evar`
within that parameter's scope; other names denote `S2Econ`. Inner parameter
bindings shadow outer bindings and constructors of the same name. A bound
static variable cannot take static arguments, since the AST has no static
variable application node.

A standalone type parser needs an explicit set of in-scope static variable
names to construct variable nodes. With an empty scope, bare names denote
constructors. In the table below, `a` and `b` are assumed to be in scope as
static variables, while `int` and `list` are constructors.
All AST sequences below use `fnlist`, preserving source order.

| Source | Level-2 tree |
| --- | --- |
| `a` or `(a)` | `S2Evar("a")` |
| `int` or `int[]` | `S2Econ("int", fnlist_nil())` |
| `list(a)` or `list[a]` | `S2Econ("list", [S2Evar("a")])` |
| `a -> b` | `S2Efun(a, b)` |
| `()` | `S2Etupl([])` |
| `(a,)` | `S2Etupl([a])` |
| `(a, b)` | `S2Etupl([a, b])` |

In mapping tables, bracketed AST children abbreviate `fnlist` construction;
letters such as `a` and `b` in tree columns stand for translated child trees.
`(a)` is allowed and means exactly `a`: parentheses group a type without
creating a tuple or adding an AST node. Nested grouping such as `((a))` also
means `a`. In contrast, `(a,)` is a singleton tuple type. Any type may be
grouped, for example `(int)`, `(list[a])`, or `(a -> b)`. Tuples and constructor
argument lists accept trailing commas.

`->` associates to the right: `a -> b -> c` means `a -> (b -> c)`.
Use `(a -> b) -> c` for a function taking a function.

The ground types are `sint`, `bool`, and `string`; the checker treats `int`
as an alias for `sint` for compatibility with the examples;
`()` is the unit type. The polymorphic constructors `list(t)` (lists) and
`strm(t)` (streams) each require exactly one element type. For example,
`list(sint)` is the type of integer lists and `strm(sint)` is the type of
integer streams. They are distinct nominal types. `optn(t)` likewise takes
one argument.

Constructor arguments may use parentheses or brackets: `list(a)` and
`list[a]` produce the same `S2Econ`. This applies to type constructors
generally, and supports nesting such as `list(strm(a))` and tuple elements
such as `strm((a,b))`. Arity is checked by the type checker, so `list()`
and `strm(a,b)` are rejected there. Type variables cannot be applied.
Names remain ordinary constructors, not parser keywords; template static
application still uses brackets, as in `@identity[list(sint)]`.

This adds type constructors only. Runtime list construction and stream
production/forcing operations are not provided by this change.

Bare `S2E000` placeholders have no source syntax. Static variables have explicit
names; there is no inference-hole or wildcard notation in this design.

## Expressions and precedence

From strongest to weakest: calls, projections, and static applications (one
shared level); unary signs; multiplication, division, and modulo; addition and
subtraction; comparisons. Postfix operations and binary arithmetic associate
left to right. Comparisons cannot be chained.

Lambda, fix, if, and let bodies extend as far right as enclosing syntax permits.
Parenthesize these forms when using them as arithmetic operands or as the
function in a call. Every `if` requires both branches.

| Source | Level-2 tree |
| --- | --- |
| `42` | `D2Eint(42)` |
| `true`, `false` | `D2Ebtf(True)`, `D2Ebtf(False)` |
| `"hello"` | `D2Estr("hello")` |
| `x` | `D2Evar("x")` |
| `@f` | `D2Ecst("f")` |
| `-e` | `D2Eop1("-", e)` |
| `+e` | `D2Eop1("+", e)` |
| `e1 + e2` | `D2Eop2("+", e1, e2)` |
| `e1 % e2` | `D2Eop2("%", e1, e2)` |
| `lam (x: t) => e` | `D2Elam("x", t, e)` |
| `fix f(x: t): r => e` | `D2Efix("f", "x", t, e, r)` |
| `f(e)` | `D2Eapp(f, e)` |
| `if c then e1 else e2` | `D2Eif0(c, e1, e2)` |
| `(e1, e2)` | `D2Etupl([e1, e2])` |
| `e.0` | `D2Eproj(0, e)` |
| `f[t1, t2]` | `D2Etapp(f, [t1, t2])` |

All listed binary operators use their literal spelling as the `D2Eop2` name.
Unary signs remain unary nodes rather than being rewritten to subtraction.
The parser does not assign arithmetic, comparison, or evaluation behavior;
the checker enforces operand types and the interpreter evaluates the operators.
Integer division is floor division; modulo follows Python remainder semantics.
Arbitrary
internal `D2Eop1`/`D2Eop2` names have no dedicated source notation yet.

`()` creates an empty tuple, `(e)` groups, and `(e,)` creates a singleton tuple.
Larger tuples and trailing commas are supported. Functions take exactly one
argument: `f(x, y)` passes a pair, `f()` passes the empty tuple, and `f(x,)`
passes a singleton tuple. `f(x)(y)` performs two applications.

Projection indices must be nonnegative integer literals. Tuple type and bounds
are checked after parsing. Note that `D2Eproj` stores the index before the
expression.

Parameter types are required. A `fix` also requires its result type; its name
and parameter must differ. The intended scope of a lambda parameter is its
body; the intended scope of both fix-bound names is the fix body. There is no
separate lambda result annotation in the current AST.

## Generic operators and constructed values

`D2CON` is a generic operator that constructs a value with a named data
constructor:

```text
D2CON("list_nil")
D2CON("list_cons", head, tail)
```

These parse as `D2Eopx("D2CON", args)`, where `args` is an ordered
`d2explst`. The first argument must be a string literal naming the constructor;
remaining arguments become its fields. Runtime evaluation returns
`D2Vcon(name, fields)`. The list library uses this to implement `list_nil` and
`list_cons`. The checker currently recognizes the list constructors `list_nil` and `list_cons`, and the option constructors `optn_nil` and `optn_cons`:
`list_nil` has result type `list(?)`, and `list_cons` infers the head type and checks
the tail has the corresponding list type. `D2TCK(value, tag)` returns whether a `D2Vcon` has that constructor name.
`D2ARG(value, index)` selects a constructor field by a zero-based integer
literal. The current checker supports these operations for list values:
for lists, index zero returns the element type and index one returns the list
type (tail); for options, index zero returns the contained type. Runtime
checks reject non-constructed values and out-of-range fields. Other `D2CON`
names and generic operators are not yet registered with the checker/interpreter.

## Bindings and let expressions

`val x = e` maps to `D2Cbind("x", e)`. The proposed binding rule is sequential:
a binding is available to later declarations and the enclosing body, but not
to its own initializer. Use `fix` for recursion. A later binding may shadow an
earlier one.

```text
let
  val x = 6;
  val answer = x * 7
in
  answer
end
```

This maps to `D2Elets(declarations, fnoptn_cons(body))`.

The body is optional, matching `d2expopt` in the AST:

```text
let
  val ignored = @print("hello")
end
```

Without `in`, the tree is `D2Elets(declarations, fnoptn_nil())`. The proposed
evaluation contract is to process the declarations and return unit. This is
distinct in the AST from an explicit `in ()` body. Both `let end` and
`let in () end` are valid; `let in end` is invalid.

## Constant signature declarations

A `dec` declares a named constant's type without providing an implementation:

```text
dec identity[a]: a -> a;
dec first[a, b]: (a, b) -> a;
dec answer: int
```

The declared name is written without `@`; expression references still use
`@identity` or `@answer`. Square brackets here bind ordered static parameters,
rather than supply static arguments. Parameters must be unique and scope only
over the signature following the colon. A bound bare name such as `a` becomes
`S2Evar("a")`, including inside constructor arguments such as `list[a]`.
It shadows a constructor of the same name within that signature. A parameter
cannot itself take static arguments: `a[int]` is invalid when `a` is bound.

Omitting the parameter list, or writing `[]`, declares zero static parameters.
Unused parameters are permitted in signatures. The declaration makes the
constant signature available to subsequent declarations and the enclosing body,
subject to the same private/exported regions as other declarations in `local`.
It does not produce a runtime value or supply an implementation.

This maps to the existing declaration variant
`D2Cdecl(arg1: d2cst, arg2: s2varlst, arg3: s2exp)` in
`FWATS3_2basics.py`. For example, using the list abbreviation above:

```text
dec identity[a]: a -> a

D2Cdecl("identity", ["a"], S2Efun(S2Evar("a"), S2Evar("a")))
```

A declaration and implementation can be written together as:

```text
dec identity[a]: a -> a;
imp{a} identity[a] = lam (x: a) => x;
val result = @identity[int](42)
```

Signature parameter names and implementation parameter names are independently
bound; they need not have the same spelling. Checking an implementation against
a signature requires the same number of static patterns as signature parameters
and substitution of those patterns into the signature type. The implementation
expression must have the resulting type. These checks are performed by `FWATS3_2tcheck.py`.

## Constants, templates, and implementations

A constant reference in an expression is written `@name`. A static application
uses square brackets:

```text
@identity[int](42)
@map[int, bool](predicate, values)
```

Static and dynamic application are separate nodes. For example,
`@f[int](x).0(y)` applies static arguments, applies one dynamic argument,
projects component zero, then applies the result to `y`. `f[]` creates a
`D2Etapp` with an empty static argument list; it is not erased. The AST permits
static application to any expression, so the grammar does too; checking whether
that expression denotes a template is a later responsibility.

Implementation declarations have the form:

```text
imp{a, b} name[pattern1, pattern2] = expression
```

The implementation name is a bare identifier, as in `dec`; `@` is used only
for constant references in expressions. The optional braces follow `imp`,
and the optional static argument brackets follow the name. Whitespace between
`imp` and the braces is permitted.

The optional braces bind static variables over both the pattern list and the
implementation expression, including its type annotations. The optional
brackets specify the static argument patterns for this implementation.
Omitting either list produces an empty `fnlist`; `[]` explicitly gives an
empty pattern list. Empty braces are not allowed: omit them instead.

```text
imp{a} identity[a] = lam (x: a) => x;
imp identity_int[int] = lam (x: int) => x;
imp answer = 42
```

The first declaration maps to:

```text
D2Cimpl(
  "identity",
  D2Elam("x", S2Evar("a"), D2Evar("x")),
  ["a"],
  [S2Evar("a")]
)
```

Static argument patterns may be compound types, including tuples:

```text
dec identity[t]: t -> t;
imp{a,b} identity[(a,b)] = lam (x: (a,b)) => x;
val pair = @identity[(int,bool)](42, true)
```

Here `{a,b}` binds two implementation parameters, while `[(a,b)]` supplies
one static argument pattern: the tuple type `(a,b)`. It matches the single
requested static argument `(int,bool)`, binding `a` to `int` and `b` to
`bool`. By contrast, `identity[a,b]` would supply two static argument
patterns and would not agree with this declaration's single signature parameter.
The number of implementation parameters need not equal the number of patterns.

The pattern list in this example is
`[S2Etupl([S2Evar("a"), S2Evar("b")])]`, using the `fnlist` abbreviation.
Likewise, `imp{a} identity[list[a]] = lam (x: list[a]) => x` uses a
constructor pattern. These illustrate separate possible implementations;
the interpreter selects the most recently declared matching implementation.

This preserves the AST field order: constant name, implementation expression,
static parameter names, static argument patterns. Parameter names must be
unique, and every declared parameter must occur in the patterns so matching
can determine its binding. Static variables must be bound by an enclosing or
current implementation, or by a signature parameter list within its signature;
standalone type parsing uses its explicitly supplied static variable scope.
These are validation rules, not grammar productions.

The proposed resolution rule matches an implementation's patterns against the
requested static arguments. It follows `s2exp_match`: only pattern-side variables
are bound, and repeated occurrences must match structurally equal types. For a
whole pattern list, matching must share one context across its elements (it can
match two `S2Etupl` wrappers). Matching elements in independent calls would lose
repeated-variable constraints. Any variables bound by an enclosing implementation
must be substituted before matching the inner implementation's patterns.

The interpreter searches implementations in reverse declaration order and uses
the first match. There is no specificity ranking or implicit static argument
inference. Each implementation captures both its variable environment and its
template environment before insertion. Matched static types are substituted
into its body, which runs in those captured environments. Use `fix` for
recursion. Functions likewise capture both environments at their definition.

## Local declarations

```text
local
  val helper = lam (x: int) => x + 1
in
  val exported = helper(41);
  imp answer = exported
end
```

This maps to `D2Clocal(private_declarations, exported_declarations)`. The proposed
scope rule makes the first sequence visible within the second, and exports only
the bindings, constant signatures, and implementations of the second sequence
to subsequent code.
Both sequences are ordered and may be empty. Functions exported from the second
sequence must retain access to private bindings they capture.

`local` is a declaration, not an expression. It can appear at top level or within
a let declaration sequence. `let` exports no bindings outside its expression.

## Complete examples

Typed recursive factorial:

```text
val fact = fix f(n: int): int =>
  if n > 0 then n * f(n - 1) else 1;
val result = fact(5)
```

Tuple argument and result types:

```text
val swap = lam (xy: (int, bool)) => (xy.1, xy.0);
val swapped = swap(42, true)
```

A generic function taking a tuple:

```text
dec first[a, b]: (a, b) -> a;
imp{a, b} first[a, b] =
  lam (xy: (a, b)) => xy.0;
val result = @first[int, bool](42, true)
```

Library operations use ordinary constants rather than reserved parser forms:

```text
val xs = @list_cons[int](1, @list_nil[int]());
val some = @optn_cons[int](42);
val ignored = @print[string]("hello")
```

These library names and signatures are illustrative; these illustrative library operations are not supplied by the current
modules. In particular, nullary library
operations are represented by application to `()`, since there is no `D2Eop0`.

## Parser and validation boundary

The parser reports syntax errors with line and column information, constructs
trees without executing code, and rejects trailing input. Examples
of syntax errors include chained comparisons, missing declaration separators,
missing required type annotations, negative projection indices, and malformed
strings.

The parser resolves static variable names using lexical parameter scopes. It
also rejects duplicate static parameters, static arguments on a bound type
variable, and identical fix/parameter names. Dynamic name resolution, type
compatibility, checking that implementation parameters occur in their patterns,
and projection bounds are handled by the type checker. Implementation matching,
selection, and runtime operator behavior are handled by the interpreter.
`CTXnil` and `CTXcns` are internal matching contexts, not source
forms. The base classes `D2E000` and `D2C000` likewise have no source syntax.
