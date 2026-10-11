"""Static checking for FWATS3 level-2 syntax (no evaluation or dispatch)."""
from dataclasses import dataclass, field
from itertools import count
from typing import Mapping

import FWATS3_2basics as a
from FWATS3_2staexp import s2exp_equal


class FWATS3TypeError(TypeError):
    """An invalid FWATS3 type, expression, or declaration."""


def _items(xs):
    result = []
    while isinstance(xs, a.fnlist_cons):
        result.append(xs.arg1)
        xs = xs.arg2
    if not isinstance(xs, a.fnlist_nil):
        raise FWATS3TypeError("Malformed functional list")
    return result


def _list(xs):
    result = a.fnlist_nil()
    for x in reversed(list(xs)):
        result = a.fnlist_cons(x, result)
    return result


def _con(name):
    return a.S2Econ(name, a.fnlist_nil())


SINT = _con("sint")
BOOL = _con("bool")
STRING = _con("string")
UNIT = a.S2Etupl(a.fnlist_nil())
_fresh = count()


@dataclass(frozen=True)
class TemplateSignature:
    # Internal unique names prevent capture by nested static binders.
    parameters: tuple[str, ...]
    body: a.s2exp


@dataclass
class TypeEnvironment:
    """Checked bindings; public checking calls copy rather than mutate this."""
    variables: dict[str, a.s2exp] = field(default_factory=dict)
    constants: dict[str, TemplateSignature] = field(default_factory=dict)
    implementations: dict[str, tuple[a.D2Cimpl, ...]] = field(default_factory=dict)

    def copy(self):
        return TypeEnvironment(dict(self.variables), dict(self.constants),
                               dict(self.implementations))


def _substitute(t, bindings):
    if isinstance(t, a.S2Evar):
        return bindings.get(t.arg1, t)
    if isinstance(t, a.S2Econ):
        return a.S2Econ(t.arg1, _list(_substitute(x, bindings) for x in _items(t.arg2)))
    if isinstance(t, a.S2Efun):
        return a.S2Efun(_substitute(t.arg1, bindings), _substitute(t.arg2, bindings))
    if isinstance(t, a.S2Etupl):
        return a.S2Etupl(_list(_substitute(x, bindings) for x in _items(t.arg1)))
    raise FWATS3TypeError(f"Unsupported static type: {type(t).__name__}")


def _variables(t):
    if isinstance(t, a.S2Evar):
        return {t.arg1}
    if isinstance(t, a.S2Efun):
        return _variables(t.arg1) | _variables(t.arg2)
    children = _items(t.arg2 if isinstance(t, a.S2Econ) else t.arg1)
    return set().union(*(_variables(x) for x in children))


def _show(t):
    if isinstance(t, a.S2Evar):
        return t.arg1.split("#", 1)[0]
    if isinstance(t, a.S2Econ):
        args = _items(t.arg2)
        return t.arg1 + ("[" + ", ".join(map(_show, args)) + "]" if args else "")
    if isinstance(t, a.S2Efun):
        return f"({_show(t.arg1)} -> {_show(t.arg2)})"
    if isinstance(t, a.S2Etupl):
        xs = _items(t.arg1)
        return "(" + ", ".join(map(_show, xs)) + ("," if len(xs) == 1 else "") + ")"
    return repr(t)


def _stream_thunk(t):
    if isinstance(t, a.S2Econ) and t.arg1 == "strm":
        args = _items(t.arg2)
        if len(args) == 1:
            stcn = a.S2Econ("stcn", _list(args))
            return a.S2Efun(UNIT, stcn)
    return None


def _compatible(actual, expected):
    actual_stream = _stream_thunk(actual)
    expected_stream = _stream_thunk(expected)
    if actual_stream is not None and expected_stream is None:
        return _compatible(actual_stream, expected)
    if expected_stream is not None and actual_stream is None:
        return _compatible(actual, expected_stream)
    if type(actual) is a.S2E000 or type(expected) is a.S2E000:
        return True
    if isinstance(actual, a.S2Econ) and isinstance(expected, a.S2Econ):
        return (actual.arg1 == expected.arg1
                and len(_items(actual.arg2)) == len(_items(expected.arg2))
                and all(_compatible(x, y) for x, y in
                        zip(_items(actual.arg2), _items(expected.arg2))))
    if isinstance(actual, a.S2Efun) and isinstance(expected, a.S2Efun):
        return (_compatible(actual.arg1, expected.arg1)
                and _compatible(actual.arg2, expected.arg2))
    if isinstance(actual, a.S2Etupl) and isinstance(expected, a.S2Etupl):
        return (len(_items(actual.arg1)) == len(_items(expected.arg1))
                and all(_compatible(x, y) for x, y in
                        zip(_items(actual.arg1), _items(expected.arg1))))
    return s2exp_equal(actual, expected)


def _equal(actual, expected, where):
    if not _compatible(actual, expected):
        raise FWATS3TypeError(f"{where}: expected {_show(expected)}, got {_show(actual)}")


class _Checker:
    def type(self, t, scope):
        if isinstance(t, a.S2Evar):
            if t.arg1 not in scope:
                raise FWATS3TypeError(f"Unbound static variable {t.arg1!r}")
            return scope[t.arg1]
        if isinstance(t, a.S2Econ):
            name = "sint" if t.arg1 == "int" else t.arg1
            args = [self.type(x, scope) for x in _items(t.arg2)]
            arity = {"sint": 0, "bool": 0, "string": 0, "list": 1, "strm": 1, "stcn": 1, "optn": 1}.get(name)
            if arity is not None and len(args) != arity:
                raise FWATS3TypeError(f"Type constructor {name!r} expects {arity} arguments")
            return a.S2Econ(name, _list(args))
        if isinstance(t, a.S2Efun):
            return a.S2Efun(self.type(t.arg1, scope), self.type(t.arg2, scope))
        if isinstance(t, a.S2Etupl):
            return a.S2Etupl(_list(self.type(x, scope) for x in _items(t.arg1)))
        raise FWATS3TypeError(f"Unsupported static type: {type(t).__name__}")

    def bind(self, parameters, scope):
        names = _items(parameters)
        if len(names) != len(set(names)):
            raise FWATS3TypeError("Duplicate static parameters")
        local = dict(scope)
        fresh = []
        for name in names:
            unique = f"{name}#{next(_fresh)}"
            fresh.append(unique)
            local[name] = a.S2Evar(unique)
        return local, tuple(fresh)

    def signature(self, name, env):
        if name not in env.constants:
            raise FWATS3TypeError(f"Undeclared template constant {name!r}")
        return env.constants[name]

    def instantiate(self, signature, args, name):
        if len(args) != len(signature.parameters):
            raise FWATS3TypeError(
                f"Template {name!r} expects {len(signature.parameters)} static arguments, "
                f"got {len(args)}")
        return _substitute(signature.body, dict(zip(signature.parameters, args)))

    def expression(self, e, env, scope):
        if isinstance(e, a.D2Eint):
            return SINT
        if isinstance(e, a.D2Ebtf):
            return BOOL
        if isinstance(e, a.D2Estr):
            return STRING
        if isinstance(e, a.D2Evar):
            if e.arg1 not in env.variables:
                raise FWATS3TypeError(f"Unbound variable {e.arg1!r}")
            return env.variables[e.arg1]
        if isinstance(e, a.D2Ecst):
            return self.instantiate(self.signature(e.arg1, env), [], e.arg1)
        if isinstance(e, a.D2Etapp):
            if not isinstance(e.arg1, a.D2Ecst):
                raise FWATS3TypeError("Static application requires a template constant")
            name = e.arg1.arg1
            args = [self.type(t, scope) for t in _items(e.arg2)]
            return self.instantiate(self.signature(name, env), args, name)
        if isinstance(e, a.D2Eopx):
            args = _items(e.arg1)
            if e.name == "D2CON":
                if not args or not isinstance(args[0], a.D2Estr):
                    raise FWATS3TypeError("D2CON requires a string constructor name first")
                constructor = args[0].arg1
                if constructor == "list_nil" and len(args) == 1:
                    return a.S2Econ("list", _list([a.S2E000()]))
                if constructor == "list_cons" and len(args) == 3:
                    head_type = self.expression(args[1], env, scope)
                    tail_type = self.expression(args[2], env, scope)
                    expected = a.S2Econ("list", _list([head_type]))
                    _equal(tail_type, expected, "D2CON list_cons tail")
                    return expected
                if constructor == "optn_nil" and len(args) == 1:
                    return a.S2Econ("optn", _list([a.S2E000()]))
                if constructor == "optn_cons" and len(args) == 2:
                    value_type = self.expression(args[1], env, scope)
                    return a.S2Econ("optn", _list([value_type]))
                if constructor == "stcn_nil" and len(args) == 1:
                    return a.S2Econ("stcn", _list([a.S2E000()]))
                if constructor == "stcn_cons" and len(args) == 3:
                    head_type = self.expression(args[1], env, scope)
                    tail_type = self.expression(args[2], env, scope)
                    expected_tail = a.S2Econ("strm", _list([head_type]))
                    _equal(tail_type, expected_tail, "D2CON stcn_cons tail")
                    return a.S2Econ("stcn", _list([head_type]))
                raise FWATS3TypeError(f"Invalid D2CON constructor or arity for {constructor!r}")
            if e.name == "D2TCK":
                if len(args) != 2 or not isinstance(args[1], a.D2Estr):
                    raise FWATS3TypeError("D2TCK requires a value followed by a string tag")
                value_type = self.expression(args[0], env, scope)
                if not isinstance(value_type, a.S2Econ) or value_type.arg1 not in {"list", "optn", "stcn"}:
                    raise FWATS3TypeError("D2TCK requires a constructed value")
                return BOOL
            if e.name == "D2ARG":
                if len(args) != 2:
                    raise FWATS3TypeError("D2ARG requires a value and a field index")
                value_type = self.expression(args[0], env, scope)
                index = args[1]
                if not isinstance(index, a.D2Eint):
                    raise FWATS3TypeError("D2ARG field index must be an integer literal")
                if isinstance(value_type, a.S2Econ) and value_type.arg1 in {"list", "optn", "stcn"}:
                    element_types = _items(value_type.arg2)
                    if len(element_types) != 1:
                        raise FWATS3TypeError(f"Malformed {value_type.arg1} type")
                    if value_type.arg1 == "list" and index.arg1 in (0, 1):
                        return element_types[0] if index.arg1 == 0 else value_type
                    if value_type.arg1 == "optn" and index.arg1 == 0:
                        return element_types[0]
                    if value_type.arg1 == "stcn" and index.arg1 == 0:
                        return element_types[0]
                    if value_type.arg1 == "stcn" and index.arg1 == 1:
                        return a.S2Econ("strm", _list(element_types))
                    raise FWATS3TypeError(f"Invalid {value_type.arg1} constructor field index")
                raise FWATS3TypeError("D2ARG requires a constructed value with known fields")
            raise FWATS3TypeError(f"Unknown generic operator {e.name!r}")
        if isinstance(e, a.D2Eop1):
            if e.name not in {"+", "-"}:
                raise FWATS3TypeError(f"Unknown unary operator {e.name!r}")
            _equal(self.expression(e.arg1, env, scope), SINT, f"Operand of {e.name}")
            return SINT
        if isinstance(e, a.D2Eop2):
            if e.name not in {"+", "-", "*", "/", "%", "<", ">", "<=", ">=", "==", "!="}:
                raise FWATS3TypeError(f"Unknown binary operator {e.name!r}")
            left = self.expression(e.arg1, env, scope)
            right = self.expression(e.arg2, env, scope)
            if e.name in {"==", "!="}:
                if not any(s2exp_equal(left, t) for t in (SINT, BOOL, STRING)):
                    raise FWATS3TypeError("Primitive equality requires sint, bool, or string")
                _equal(right, left, f"Right operand of {e.name}")
            else:
                _equal(left, SINT, f"Left operand of {e.name}")
                _equal(right, SINT, f"Right operand of {e.name}")
            return SINT if e.name in {"+", "-", "*", "/", "%"} else BOOL
        if isinstance(e, a.D2Elam):
            argument = self.type(e.arg2, scope)
            inner = env.copy()
            inner.variables[e.arg1] = argument
            return a.S2Efun(argument, self.expression(e.arg3, inner, scope))
        if isinstance(e, a.D2Efix):
            if e.arg1 == e.arg2:
                raise FWATS3TypeError("Fix name and parameter must differ")
            argument = self.type(e.arg3, scope)
            result = self.type(e.arg5, scope)
            function = a.S2Efun(argument, result)
            inner = env.copy()
            inner.variables.update({e.arg1: function, e.arg2: argument})
            _equal(self.expression(e.arg4, inner, scope), result, "Fix body")
            return function
        if isinstance(e, a.D2Eapp):
            function = self.expression(e.arg1, env, scope)
            if isinstance(function, a.S2Econ) and function.arg1 == "strm":
                stream_args = _items(function.arg2)
                if len(stream_args) != 1:
                    raise FWATS3TypeError("Malformed stream type")
                _equal(self.expression(e.arg2, env, scope), UNIT, "Stream force argument")
                return a.S2Econ("stcn", _list(stream_args))
            if not isinstance(function, a.S2Efun):
                raise FWATS3TypeError(f"Application requires a function, got {_show(function)}")
            _equal(self.expression(e.arg2, env, scope), function.arg1, "Function argument")
            return function.arg2
        if isinstance(e, a.D2Eif0):
            _equal(self.expression(e.arg1, env, scope), BOOL, "If condition")
            yes = self.expression(e.arg2, env, scope)
            _equal(self.expression(e.arg3, env, scope), yes, "If branches")
            return yes
        if isinstance(e, a.D2Etupl):
            return a.S2Etupl(_list(self.expression(x, env, scope) for x in _items(e.arg1)))
        if isinstance(e, a.D2Eproj):
            t = self.expression(e.arg2, env, scope)
            if not isinstance(t, a.S2Etupl):
                raise FWATS3TypeError("Projection requires a tuple")
            components = _items(t.arg1)
            if type(e.arg1) is not int or not 0 <= e.arg1 < len(components):
                raise FWATS3TypeError(f"Tuple projection index {e.arg1!r} is out of bounds")
            return components[e.arg1]
        if isinstance(e, a.D2Elets):
            inner, _ = self.declarations(e.arg1, env, scope)
            if isinstance(e.arg2, a.fnoptn_nil):
                return UNIT
            if isinstance(e.arg2, a.fnoptn_cons):
                return self.expression(e.arg2.arg1, inner, scope)
            raise FWATS3TypeError("Malformed let body option")
        raise FWATS3TypeError(f"Unsupported expression: {type(e).__name__}")

    def declarations(self, declarations, env, scope):
        result = env.copy()
        # Explicit exports are needed even when a binding shadows an equal type.
        exports = TypeEnvironment()
        for declaration in _items(declarations):
            try:
                self.declaration(declaration, result, scope, exports)
            except FWATS3TypeError as error:
                name = getattr(declaration, "arg1", "")
                label = repr(name) if isinstance(name, str) else "local"
                raise FWATS3TypeError(f"{declaration.ctag} {label}: {error}") from error
        return result, exports

    def declaration(self, d, env, scope, exports):
        if isinstance(d, a.D2Cbind):
            t = self.expression(d.arg2, env, scope)
            env.variables[d.arg1] = exports.variables[d.arg1] = t
        elif isinstance(d, a.D2Cdecl):
            # No implicit replacement of a signature with existing implementations.
            if d.arg1 in env.constants:
                raise FWATS3TypeError(f"Duplicate constant declaration {d.arg1!r}")
            local, params = self.bind(d.arg2, scope)
            signature = TemplateSignature(params, self.type(d.arg3, local))
            env.constants[d.arg1] = exports.constants[d.arg1] = signature
        elif isinstance(d, a.D2Cimpl):
            signature = self.signature(d.arg1, env)
            local, params = self.bind(d.arg3, scope)
            patterns = [self.type(t, local) for t in _items(d.arg4)]
            used = set().union(*(_variables(t) for t in patterns))
            if set(params) - used:
                raise FWATS3TypeError("Every implementation parameter must occur in its patterns")
            expected = self.instantiate(signature, patterns, d.arg1)
            _equal(self.expression(d.arg2, env, local), expected, "Implementation")
            env.implementations[d.arg1] = env.implementations.get(d.arg1, ()) + (d,)
            exports.implementations[d.arg1] = exports.implementations.get(d.arg1, ()) + (d,)
        elif isinstance(d, a.D2Clocal):
            private, _ = self.declarations(d.arg1, env, scope)
            _, public = self.declarations(d.arg2, private, scope)
            # An implementation of a private signature remains private as well.
            for name, implementations in public.implementations.items():
                if name in env.constants or name in public.constants:
                    env.implementations[name] = env.implementations.get(name, ()) + implementations
                    exports.implementations[name] = exports.implementations.get(name, ()) + implementations
            env.variables.update(public.variables)
            env.constants.update(public.constants)
            exports.variables.update(public.variables)
            exports.constants.update(public.constants)
        else:
            raise FWATS3TypeError(f"Unsupported declaration: {type(d).__name__}")


def d2exp_tcheck(expression: a.d2exp, env: TypeEnvironment | None = None,
                 *, static_vars: Mapping[str, a.s2exp] | None = None) -> a.s2exp:
    """Infer/check an expression; return its normalized static type."""
    return _Checker().expression(expression, env or TypeEnvironment(), dict(static_vars or {}))


def d2eclist_tcheck(declarations: a.d2eclist,
                    env: TypeEnvironment | None = None) -> TypeEnvironment:
    """Check sequential declarations and return an extended, independent environment."""
    return _Checker().declarations(declarations, env or TypeEnvironment(), {})[0]


def program_tcheck(program: a.D2Elets, env: TypeEnvironment | None = None) -> a.s2exp:
    """Check a parsed program (a let tree with a required result expression)."""
    if not isinstance(program, a.D2Elets) or not isinstance(program.arg2, a.fnoptn_cons):
        raise FWATS3TypeError("A program requires declarations and a final expression")
    return d2exp_tcheck(program, env)


libfile_tcheck = d2eclist_tcheck
