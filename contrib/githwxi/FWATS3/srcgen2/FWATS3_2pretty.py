"""Pretty-print FWATS3 level-2 syntax as parseable source text."""
import FWATS3_2basics as a


class FWATS3PrettyError(ValueError):
    """An AST node has no representation in the FWATS3 source syntax."""


def _items(xs):
    while isinstance(xs, a.fnlist_cons):
        yield xs.arg1
        xs = xs.arg2
    if not isinstance(xs, a.fnlist_nil):
        raise FWATS3PrettyError("Malformed functional list")


def _comma(values):
    return ", ".join(values)


def s2exp_pretty(expr: a.s2exp) -> str:
    """Format a static expression/type."""
    if isinstance(expr, a.S2Evar):
        return expr.arg1
    if isinstance(expr, a.S2Econ):
        args = list(_items(expr.arg2))
        return expr.arg1 if not args else f"{expr.arg1}[{_comma(s2exp_pretty(x) for x in args)}]"
    if isinstance(expr, a.S2Efun):
        left = s2exp_pretty(expr.arg1)
        if isinstance(expr.arg1, a.S2Efun):
            left = f"({left})"
        return f"{left} -> {s2exp_pretty(expr.arg2)}"
    if isinstance(expr, a.S2Etupl):
        parts = [s2exp_pretty(x) for x in _items(expr.arg1)]
        if len(parts) == 1:
            return f"({parts[0]},)"
        return f"({ _comma(parts) })"
    raise FWATS3PrettyError(f"Unsupported static expression {type(expr).__name__}")


def _static_params(params) -> str:
    names = list(_items(params))
    return "[" + _comma(names) + "]" if names else ""


def _static_args(args) -> str:
    values = list(_items(args))
    return "[" + _comma(s2exp_pretty(x) for x in values) + "]"


def d2ecl_pretty(declaration: a.d2ecl, indent: int = 0) -> str:
    """Format one declaration."""
    pad = " " * indent
    if isinstance(declaration, a.D2Cbind):
        return f"{pad}val {declaration.arg1} = {d2exp_pretty(declaration.arg2)}"
    if isinstance(declaration, a.D2Cdecl):
        return (f"{pad}dec {declaration.arg1}{_static_params(declaration.arg2)}: "
                f"{s2exp_pretty(declaration.arg3)}")
    if isinstance(declaration, a.D2Cimpl):
        params = list(_items(declaration.arg3))
        patterns = _static_args(declaration.arg4)
        binder = "{" + _comma(params) + "}" if params else ""
        return (f"{pad}imp{binder} {declaration.arg1}{patterns} = "
                f"{d2exp_pretty(declaration.arg2)}")
    if isinstance(declaration, a.D2Clocal):
        private = d2eclist_pretty(declaration.arg1, indent + 2)
        public = d2eclist_pretty(declaration.arg2, indent + 2)
        return (f"{pad}local\n{private}\n{pad}in\n{public}\n{pad}end")
    raise FWATS3PrettyError(f"Unsupported declaration {type(declaration).__name__}")


def d2eclist_pretty(declarations: a.d2eclist, indent: int = 0) -> str:
    """Format declarations in source order, separated by semicolons."""
    return ";\n".join(d2ecl_pretty(d, indent) for d in _items(declarations))


def d2exp_pretty(expr: a.d2exp) -> str:
    """Format an expression with grouping that preserves its AST structure."""
    if isinstance(expr, a.D2Eint):
        if expr.arg1 < 0:
            return f"(-{abs(expr.arg1)})"
        return str(expr.arg1)
    if isinstance(expr, a.D2Ebtf):
        return "true" if expr.arg1 else "false"
    if isinstance(expr, a.D2Estr):
        escaped = (expr.arg1.replace("\\", "\\\\").replace('"', '\\"')
                   .replace("\n", "\\n").replace("\r", "\\r")
                   .replace("\t", "\\t"))
        return f'"{escaped}"'
    if isinstance(expr, a.D2Evar):
        return expr.arg1
    if isinstance(expr, a.D2Ecst):
        return "@" + expr.arg1
    if isinstance(expr, a.D2Etapp):
        return f"{d2exp_pretty(expr.arg1)}{_static_args(expr.arg2)}"
    if isinstance(expr, a.D2Eop1):
        return f"({expr.name}{d2exp_pretty(expr.arg1)})"
    if isinstance(expr, a.D2Eop2):
        return (f"({d2exp_pretty(expr.arg1)} {expr.name} "
                f"{d2exp_pretty(expr.arg2)})")
    if isinstance(expr, a.D2Eopx):
        args = _comma(d2exp_pretty(x) for x in _items(expr.arg1))
        return f"{expr.name}({args})"
    if isinstance(expr, a.D2Elam):
        return (f"lam ({expr.arg1}: {s2exp_pretty(expr.arg2)}) => "
                f"{d2exp_pretty(expr.arg3)}")
    if isinstance(expr, a.D2Efix):
        return (f"fix {expr.arg1}({expr.arg2}: {s2exp_pretty(expr.arg3)}): "
                f"{s2exp_pretty(expr.arg5)} => {d2exp_pretty(expr.arg4)}")
    if isinstance(expr, a.D2Eapp):
        return f"({d2exp_pretty(expr.arg1)})({d2exp_pretty(expr.arg2)})"
    if isinstance(expr, a.D2Eif0):
        return (f"(if {d2exp_pretty(expr.arg1)} then {d2exp_pretty(expr.arg2)} "
                f"else {d2exp_pretty(expr.arg3)})")
    if isinstance(expr, a.D2Etupl):
        parts = [d2exp_pretty(x) for x in _items(expr.arg1)]
        if len(parts) == 1:
            return f"({parts[0]},)"
        return f"({ _comma(parts) })"
    if isinstance(expr, a.D2Eproj):
        return f"({d2exp_pretty(expr.arg2)}).{expr.arg1}"
    if isinstance(expr, a.D2Elets):
        declarations = d2eclist_pretty(expr.arg1, 2)
        body = ""
        if isinstance(expr.arg2, a.fnoptn_cons):
            body = f"\n in\n  {d2exp_pretty(expr.arg2.arg1)}"
        elif not isinstance(expr.arg2, a.fnoptn_nil):
            raise FWATS3PrettyError("Malformed optional let body")
        return f"let\n{declarations}{body}\nend"
    raise FWATS3PrettyError(f"Unsupported expression {type(expr).__name__}")


def program_pretty(program: a.D2Elets) -> str:
    """Format a complete program (declarations followed by its result)."""
    if not isinstance(program, a.D2Elets) or not isinstance(program.arg2, a.fnoptn_cons):
        raise FWATS3PrettyError("A program requires a final expression")
    declarations = d2eclist_pretty(program.arg1)
    body = d2exp_pretty(program.arg2.arg1)
    return f"{declarations};\n{body}" if declarations else body
