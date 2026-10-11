"""FWATS3 evaluation with lexical variable and template environments."""
from dataclasses import dataclass, fields, is_dataclass
from types import MappingProxyType
from typing import Mapping
import operator

import FWATS3_2basics as a
from FWATS3_2staexp import s2exp_match


class FWATS3RuntimeError(RuntimeError):
    """Invalid runtime operation or missing template implementation."""


type denv = Mapping[str, D2VAL]
type tenv = tuple[TemplateClosure, ...]


@dataclass(frozen=True)
class D2Vcon:
    """A constructed runtime value: constructor name and ordered fields."""
    name: str
    args: a.fnlist["D2VAL"]


@dataclass(frozen=True)
class D2Vlam:
    expression: a.D2Elam
    denv: denv
    tenv: tenv


@dataclass(frozen=True)
class D2Vfix:
    expression: a.D2Efix
    denv: denv
    tenv: tenv


@dataclass(frozen=True)
class TemplateClosure:
    implementation: a.D2Cimpl
    denv: denv
    tenv: tenv


type D2VAL = int | bool | str | tuple[D2VAL, ...] | D2Vcon | D2Vlam | D2Vfix
type d2val = D2VAL


def d2val_format(value: D2VAL) -> str:
    """Return a readable representation, recursively formatting constructors."""
    if isinstance(value, D2Vcon):
        args = list(_items(value.args))
        if not args:
            return f"{value.name}()"
        return f"{value.name}(" + ", ".join(d2val_format(x) for x in args) + ")"
    if isinstance(value, tuple):
        return "(" + ", ".join(d2val_format(x) for x in value) + ("," if len(value) == 1 else "") + ")"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, str)):
        return repr(value) if isinstance(value, str) else str(value)
    if isinstance(value, D2Vlam):
        return "<lam>"
    if isinstance(value, D2Vfix):
        return "<fix>"
    raise FWATS3RuntimeError(f"Unsupported runtime value: {type(value).__name__}")


def _snapshot(environment):
    return MappingProxyType(dict(environment))


def _items(xs):
    while isinstance(xs, a.fnlist_cons):
        yield xs.arg1
        xs = xs.arg2
    if not isinstance(xs, a.fnlist_nil):
        raise FWATS3RuntimeError("Malformed functional list")


def _list(xs):
    result = a.fnlist_nil()
    for x in reversed(list(xs)):
        result = a.fnlist_cons(x, result)
    return result


def _static(t, *, ground=False):
    """Normalize int, rejecting unresolved variables in requested arguments."""
    if isinstance(t, a.S2Evar):
        if ground:
            raise FWATS3RuntimeError(f"Unresolved static variable {t.arg1!r}")
        return t
    if isinstance(t, a.S2Econ):
        return a.S2Econ("sint" if t.arg1 == "int" else t.arg1,
                        _list(_static(x, ground=ground) for x in _items(t.arg2)))
    if isinstance(t, a.S2Efun):
        return a.S2Efun(_static(t.arg1, ground=ground), _static(t.arg2, ground=ground))
    if isinstance(t, a.S2Etupl):
        return a.S2Etupl(_list(_static(x, ground=ground) for x in _items(t.arg1)))
    raise FWATS3RuntimeError(f"Unsupported static type {type(t).__name__}")


def _substitute(node, bindings):
    """Instantiate static occurrences, respecting nested dec/imp binders.

    Requested types are ground, so removing shadowed names suffices to avoid
    capture. Substitution builds new trees and never changes the source AST.
    """
    if isinstance(node, a.S2Evar):
        return bindings.get(node.arg1, node)
    if isinstance(node, a.D2Cdecl):
        inner = {k: v for k, v in bindings.items() if k not in set(_items(node.arg2))}
        return a.D2Cdecl(node.arg1, node.arg2, _substitute(node.arg3, inner))
    if isinstance(node, a.D2Cimpl):
        inner = {k: v for k, v in bindings.items() if k not in set(_items(node.arg3))}
        return a.D2Cimpl(node.arg1, _substitute(node.arg2, inner), node.arg3,
                         _substitute(node.arg4, inner))
    if isinstance(node, a.fnlist):
        return _list(_substitute(x, bindings) for x in _items(node))
    if is_dataclass(node):
        return type(node)(*(_substitute(getattr(node, f.name), bindings) for f in fields(node)))
    return node


def _integer(value):
    if type(value) is not int:
        raise FWATS3RuntimeError("Integer operand required")
    return value


_BINARY = {
    "+": operator.add, "-": operator.sub, "*": operator.mul,
    "/": operator.floordiv, "%": operator.mod,
    "<": operator.lt, ">": operator.gt, "<=": operator.le, ">=": operator.ge,
    "==": operator.eq, "!=": operator.ne,
}


class _Interpreter:
    def template(self, name, args, templates):
        requested = a.S2Etupl(_list(_static(t, ground=True) for t in _items(args)))
        # Environments are newest-first: the most recent matching imp wins.
        for closure in templates:
            declaration = closure.implementation
            if declaration.arg1 != name:
                continue
            patterns = a.S2Etupl(_list(_static(t) for t in _items(declaration.arg4)))
            parameters = list(_items(declaration.arg3))
            if len(set(parameters)) != len(parameters):
                raise FWATS3RuntimeError("Duplicate implementation parameters")
            match = s2exp_match(patterns, requested)
            if isinstance(match, a.fnoptn_nil):
                continue
            bindings = {}
            context = match.arg1
            while isinstance(context, a.CTXcns):
                bindings[context.arg1] = context.arg2
                context = context.arg3
            if set(bindings) != set(parameters):
                raise FWATS3RuntimeError(
                    f"Implementation {name!r} has unbound or unused static parameters")
            body = _substitute(declaration.arg2, bindings)
            return self.expression(body, closure.denv, closure.tenv)
        raise FWATS3RuntimeError(f"No matching implementation for template {name!r}")

    def apply(self, function, argument):
        if isinstance(function, D2Vlam):
            local = dict(function.denv)
            local[function.expression.arg1] = argument
            return self.expression(function.expression.arg3, local, function.tenv)
        if isinstance(function, D2Vfix):
            expression = function.expression
            if expression.arg1 == expression.arg2:
                raise FWATS3RuntimeError("Fix name and parameter must differ")
            local = dict(function.denv)
            local[expression.arg1] = function
            local[expression.arg2] = argument
            return self.expression(expression.arg4, local, function.tenv)
        raise FWATS3RuntimeError("Application requires a function closure")

    def expression(self, e, variables, templates):
        if isinstance(e, (a.D2Eint, a.D2Ebtf, a.D2Estr)):
            return e.arg1
        if isinstance(e, a.D2Evar):
            if e.arg1 not in variables:
                raise FWATS3RuntimeError(f"Unbound variable {e.arg1!r}")
            return variables[e.arg1]
        if isinstance(e, a.D2Ecst):
            return self.template(e.arg1, a.fnlist_nil(), templates)
        if isinstance(e, a.D2Etapp):
            if not isinstance(e.arg1, a.D2Ecst):
                raise FWATS3RuntimeError("Static application requires a template constant")
            return self.template(e.arg1.arg1, e.arg2, templates)
        if isinstance(e, a.D2Elam):
            return D2Vlam(e, _snapshot(variables), tuple(templates))
        if isinstance(e, a.D2Efix):
            return D2Vfix(e, _snapshot(variables), tuple(templates))
        if isinstance(e, a.D2Eapp):
            function = self.expression(e.arg1, variables, templates)
            argument = self.expression(e.arg2, variables, templates)
            return self.apply(function, argument)
        if isinstance(e, a.D2Eopx):
            args = list(_items(e.arg1))
            if e.name == "D2CON":
                if not args or not isinstance(args[0], a.D2Estr):
                    raise FWATS3RuntimeError("D2CON requires a string constructor name first")
                name = args[0].arg1
                fields = (self.expression(x, variables, templates) for x in args[1:])
                return D2Vcon(name, _list(fields))
            if e.name == "D2TCK":
                if len(args) != 2 or not isinstance(args[1], a.D2Estr):
                    raise FWATS3RuntimeError("D2TCK requires a value followed by a string tag")
                value = self.expression(args[0], variables, templates)
                if not isinstance(value, D2Vcon):
                    raise FWATS3RuntimeError("D2TCK requires a constructed value")
                return value.name == args[1].arg1
            if e.name == "D2ARG":
                if len(args) != 2:
                    raise FWATS3RuntimeError("D2ARG requires a value and a field index")
                value = self.expression(args[0], variables, templates)
                index = self.expression(args[1], variables, templates)
                if not isinstance(value, D2Vcon):
                    raise FWATS3RuntimeError("D2ARG requires a constructed value")
                if type(index) is not int or not 0 <= index < len(list(_items(value.args))):
                    raise FWATS3RuntimeError("D2ARG field index is out of bounds")
                return list(_items(value.args))[index]
            raise FWATS3RuntimeError(f"Unknown generic operator {e.name!r}")
        if isinstance(e, a.D2Eop1):
            if e.name not in {"+", "-"}:
                raise FWATS3RuntimeError(f"Unknown unary operator {e.name!r}")
            operand = _integer(self.expression(e.arg1, variables, templates))
            return operand if e.name == "+" else -operand
        if isinstance(e, a.D2Eop2):
            if e.name not in _BINARY:
                raise FWATS3RuntimeError(f"Unknown binary operator {e.name!r}")
            left = self.expression(e.arg1, variables, templates)
            right = self.expression(e.arg2, variables, templates)
            if e.name in {"==", "!="}:
                if type(left) not in (int, bool, str) or type(right) is not type(left):
                    raise FWATS3RuntimeError("Equality requires equal primitive operand types")
            else:
                _integer(left)
                _integer(right)
            try:
                return _BINARY[e.name](left, right)
            except ZeroDivisionError as error:
                raise FWATS3RuntimeError("Division or modulo by zero") from error
        if isinstance(e, a.D2Eif0):
            condition = self.expression(e.arg1, variables, templates)
            if type(condition) is not bool:
                raise FWATS3RuntimeError("If condition must be boolean")
            return self.expression(e.arg2 if condition else e.arg3, variables, templates)
        if isinstance(e, a.D2Etupl):
            return tuple(self.expression(x, variables, templates) for x in _items(e.arg1))
        if isinstance(e, a.D2Eproj):
            value = self.expression(e.arg2, variables, templates)
            if type(value) is not tuple:
                raise FWATS3RuntimeError("Projection requires a tuple")
            if type(e.arg1) is not int or not 0 <= e.arg1 < len(value):
                raise FWATS3RuntimeError(f"Tuple projection index {e.arg1!r} is out of bounds")
            return value[e.arg1]
        if isinstance(e, a.D2Elets):
            local, implementations, _, _ = self.declarations(e.arg1, variables, templates)
            if isinstance(e.arg2, a.fnoptn_nil):
                return ()
            if isinstance(e.arg2, a.fnoptn_cons):
                return self.expression(e.arg2.arg1, local, implementations)
            raise FWATS3RuntimeError("Malformed let body option")
        raise FWATS3RuntimeError(f"Unsupported expression {type(e).__name__}")

    def declarations(self, declarations, variables, templates):
        local = dict(variables)
        implementations = tuple(templates)
        exported_variables = {}
        exported_templates = ()
        for d in _items(declarations):
            if isinstance(d, a.D2Cbind):
                value = self.expression(d.arg2, local, implementations)
                local[d.arg1] = exported_variables[d.arg1] = value
            elif isinstance(d, a.D2Cdecl):
                # Signatures have no runtime value; check them before evaluation.
                continue
            elif isinstance(d, a.D2Cimpl):
                closure = TemplateClosure(d, _snapshot(local), implementations)
                implementations = (closure,) + implementations
                exported_templates = (closure,) + exported_templates
            elif isinstance(d, a.D2Clocal):
                private_denv, private_tenv, _, _ = self.declarations(
                    d.arg1, local, implementations)
                _, _, public_denv, public_tenv = self.declarations(
                    d.arg2, private_denv, private_tenv)
                local.update(public_denv)
                exported_variables.update(public_denv)
                implementations = public_tenv + implementations
                exported_templates = public_tenv + exported_templates
            else:
                raise FWATS3RuntimeError(f"Unsupported declaration {type(d).__name__}")
        return _snapshot(local), implementations, exported_variables, exported_templates


def d2exp_evaluate(expression: a.d2exp, denv: denv | None = None,
                    tenv: tenv = ()) -> d2val:
    """Evaluate an expression; functions and templates use captured environments."""
    return _Interpreter().expression(expression, _snapshot(denv or {}), tuple(tenv))


def d2eclist_evaluate(declarations: a.d2eclist, denv: denv | None = None,
                       tenv: tenv = ()) -> tuple[denv, tenv]:
    """Evaluate declarations, returning new (denv, tenv) without mutating inputs."""
    variables, templates, _, _ = _Interpreter().declarations(
        declarations, denv or {}, tuple(tenv))
    return variables, templates


def program_evaluate(program: a.D2Elets, denv: denv | None = None,
                     tenv: tenv = ()) -> d2val:
    if not isinstance(program, a.D2Elets) or not isinstance(program.arg2, a.fnoptn_cons):
        raise FWATS3RuntimeError("A program requires a final expression")
    return d2exp_evaluate(program, denv, tenv)


libfile_evaluate = d2eclist_evaluate
