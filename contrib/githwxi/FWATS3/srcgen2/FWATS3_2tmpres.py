"""Static resolution of FWATS3 template uses.

Template implementations are selected while walking declarations, with the
newest matching implementation taking precedence. A selected implementation
is expanded at its use site, so template uses inside its body see the active
template environment at that point (the interpreter's dynamic resolution
rule).
"""
from dataclasses import fields, is_dataclass

import FWATS3_2basics as a
from FWATS3_2staexp import s2exp_match


class FWATS3TemplateResolutionError(ValueError):
    """A template use has no implementation in the active environment."""


def _items(xs):
    while isinstance(xs, a.fnlist_cons):
        yield xs.arg1
        xs = xs.arg2
    if not isinstance(xs, a.fnlist_nil):
        raise FWATS3TemplateResolutionError("Malformed functional list")


def _list(items):
    result = a.fnlist_nil()
    for item in reversed(list(items)):
        result = a.fnlist_cons(item, result)
    return result


def _substitute(node, bindings):
    """Substitute static variables in an implementation body."""
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
        return type(node)(*(_substitute(getattr(node, f.name), bindings)
                            for f in fields(node)))
    return node


class _Resolver:
    def expression(self, expr, templates):
        if isinstance(expr, a.D2Ecst):
            return self.use(expr.arg1, a.fnlist_nil(), templates)
        if isinstance(expr, a.D2Etapp):
            if not isinstance(expr.arg1, a.D2Ecst):
                raise FWATS3TemplateResolutionError(
                    "Static application requires a template constant")
            return self.use(expr.arg1.arg1, expr.arg2, templates)
        if isinstance(expr, a.D2Elam):
            return a.D2Elam(expr.arg1, expr.arg2,
                            self.expression(expr.arg3, templates))
        if isinstance(expr, a.D2Efix):
            return a.D2Efix(expr.arg1, expr.arg2, expr.arg3,
                            self.expression(expr.arg4, templates), expr.arg5)
        if isinstance(expr, a.D2Elets):
            declarations, active = self.declarations(expr.arg1, templates)
            if isinstance(expr.arg2, a.fnoptn_cons):
                body = a.fnoptn_cons(self.expression(expr.arg2.arg1, active))
            else:
                body = expr.arg2
            return a.D2Elets(declarations, body)
        if isinstance(expr, a.fnlist):
            return _list(self.expression(x, templates) for x in _items(expr))
        if is_dataclass(expr):
            return type(expr)(*(self.expression(getattr(expr, f.name), templates)
                                if isinstance(getattr(expr, f.name), a.D2E000)
                                or isinstance(getattr(expr, f.name), a.fnlist)
                                else getattr(expr, f.name)
                                for f in fields(expr)))
        return expr

    def use(self, name, arguments, templates):
        requested = a.S2Etupl(_list(_items(arguments)))
        for implementation in templates:
            if implementation.arg1 != name:
                continue
            pattern = a.S2Etupl(implementation.arg4)
            matched = s2exp_match(pattern, requested)
            if isinstance(matched, a.fnoptn_nil):
                continue
            bindings = {}
            context = matched.arg1
            while isinstance(context, a.CTXcns):
                bindings[context.arg1] = context.arg2
                context = context.arg3
            if set(bindings) != set(_items(implementation.arg3)):
                raise FWATS3TemplateResolutionError(
                    f"Implementation {name!r} has unbound or unused parameters")
            body = _substitute(implementation.arg2, bindings)
            expanded = self.expression(body, templates)
            if isinstance(expanded, (a.D2Ecst, a.D2Etapp)):
                raise FWATS3TemplateResolutionError(
                    f"Could not resolve template {name!r}")
            return expanded
        raise FWATS3TemplateResolutionError(
            f"No matching implementation for template {name!r}")

    def declarations(self, declarations, templates):
        output = []
        active = tuple(templates)
        for declaration in _items(declarations):
            if isinstance(declaration, a.D2Cimpl):
                active = (declaration,) + active
            elif isinstance(declaration, a.D2Cdecl):
                output.append(declaration)
            elif isinstance(declaration, a.D2Cbind):
                output.append(a.D2Cbind(
                    declaration.arg1, self.expression(declaration.arg2, active)))
            else:
                raise FWATS3TemplateResolutionError(
                    f"Unsupported declaration {type(declaration).__name__}")
        return _list(output), active


def d2exp_tmpres(expression: a.d2exp) -> a.d2exp:
    """Translate an expression into one without template uses.

    For a program represented as ``D2Elets``, implementations are resolved
    according to declaration order and lexical scope. Missing implementations
    raise :class:`FWATS3TemplateResolutionError`.
    """
    return _Resolver().expression(expression, ())


def program_tmpres(program: a.D2Elets) -> a.D2Elets:
    """Resolve template uses in a complete program expression."""
    if not isinstance(program, a.D2Elets) or not isinstance(program.arg2, a.fnoptn_cons):
        raise FWATS3TemplateResolutionError("A program requires a final expression")
    result = d2exp_tmpres(program)
    assert isinstance(result, a.D2Elets)
    return result
