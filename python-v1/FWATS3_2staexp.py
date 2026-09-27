########################################################################

"""
Structural operations on level-2 static expressions.
"""
########################################################################

from abc import ABC
from \
dataclasses import dataclass

from FWATS3_2basics import (
    S2E000, S2Econ, S2Evar, S2Efun, S2Etupl,
    D2E000, D2Eint, D2Ebtf, D2Estr, D2Eop1, D2Eop2, D2Evar, D2Ecst,
    D2Elam, D2Efix, D2Eapp, D2Eif0, D2Etupl, D2Eproj, D2Elets, D2Etapp,
    D2Cbind, D2Cimpl, D2Clocal,
    strn, s2exp, d2ecl, s2explst, fnlist, fnlist_cons, fnlist_nil,
    d2exp, d2expopt, d2explst, d2eclist,
    fnoptn, fnoptn_cons, fnoptn_nil,
)

########################################################################
@dataclass\
(frozen=True)
class CTX000(ABC):
    ctag = "CTX000"
    pass
type s2ctx = CTX000
########################################################################
#
@dataclass\
(frozen=True)
class CTXnil(CTX000):
    pass
#
@dataclass\
(frozen=True)
class CTXcns(CTX000):
    arg1: strn
    arg2: s2exp | d2ecl
    arg3: s2ctx
    ctag = "CTXcns"
#
########################################################################

def s2exp_equal(s2el: s2exp, s2er: s2exp) -> bool:
    """
    Compare constructor kinds, names, and ordered children structurally.
    Variables compare by name, without renaming or unification. Two bare
    S2E000 placeholders compare equal, but never equal a concrete type.
    """
    if False:
        return False
    elif isinstance(s2el, S2Evar):
        if isinstance(s2er, S2Evar):
            return s2el.arg1 == s2er.arg1
        else:
            return False
    elif isinstance(s2el, S2Econ):
        if isinstance(s2er, S2Econ):
            return (s2el.arg1 == s2er.arg1
                    and s2explst_equal(s2el.arg2, s2er.arg2))
        else:
            return False
    elif isinstance(s2el, S2Efun):
        if isinstance(s2er, S2Efun):
            return (s2exp_equal(s2el.arg1, s2er.arg1)
                    and s2exp_equal(s2el.arg2, s2er.arg2))
        else:
            return False
    elif isinstance(s2el, S2Etupl):
        if isinstance(s2er, S2Etupl):
            return s2explst_equal(s2el.arg1, s2er.arg1)
        else:
            return False
    elif type(s2el) is S2E000:
        return type(s2er) is S2E000
    raise TypeError(f"Unsupported static expression: {type(s2el).__name__}")

def s2explst_equal(s2el: s2explst, s2er: s2explst) -> bool:
    while isinstance(s2el, fnlist_cons):
        if isinstance(s2er, fnlist_cons):
            if not s2exp_equal(s2el.arg1, s2er.arg1):
                return False
            else:
                s2el, s2er = s2el.arg2, s2er.arg2
        else:
            return False
    return isinstance(s2el, fnlist_nil) and isinstance(s2er, fnlist_nil)

########################################################################

def s2exp_match(s2el: s2exp, s2er: s2exp) -> fnoptn[s2ctx]:
    """
    Match the left pattern against the right type, binding left variables.
    Right variables are literal types; repeated bindings must be equal.
    Success wraps a context (possibly empty); failure returns fnoptn_nil.
    Bindings are prepended in left-to-right traversal order. Inputs are
    unchanged, and each call starts with an empty context.
    """
    def f0_s2exp(s2el: s2exp, s2er: s2exp, ctx: s2ctx) -> fnoptn[s2ctx]:
        if False:
            return fnoptn_nil()
        elif isinstance(s2el, S2Evar):
            rest = ctx
            while isinstance(rest, CTXcns):
                if s2el.arg1 == rest.arg1:
                    if isinstance(rest.arg2, S2E000):
                        if s2exp_equal(rest.arg2, s2er):
                            return fnoptn_cons(ctx)
                        else:
                            return fnoptn_nil()
                    else:
                        return fnoptn_nil()
                else:
                    rest = rest.arg3
            return fnoptn_cons(CTXcns(s2el.arg1, s2er, ctx))
        elif isinstance(s2el, S2Econ):
            if isinstance(s2er, S2Econ):
                if s2el.arg1 == s2er.arg1:
                    return f0_s2explst(s2el.arg2, s2er.arg2, ctx)
                else:
                    return fnoptn_nil()
            else:
                return fnoptn_nil()
        elif isinstance(s2el, S2Efun):
            if isinstance(s2er, S2Efun):
                result = f0_s2exp(s2el.arg1, s2er.arg1, ctx)
                if isinstance(result, fnoptn_cons):
                    return f0_s2exp(s2el.arg2, s2er.arg2, result.arg1)
                else:
                    return fnoptn_nil()
            else:
                return fnoptn_nil()
        elif isinstance(s2el, S2Etupl):
            if isinstance(s2er, S2Etupl):
                return f0_s2explst(s2el.arg1, s2er.arg1, ctx)
            else:
                return fnoptn_nil()
        elif type(s2el) is S2E000:
            if type(s2er) is S2E000:
                return fnoptn_cons(ctx)
            else:
                return fnoptn_nil()
        raise TypeError(f"Unsupported static expression: {type(s2el).__name__}")

    def f0_s2explst(s2el: s2explst, s2er: s2explst,
                   ctx: s2ctx) -> fnoptn[s2ctx]:
        while isinstance(s2el, fnlist_cons):
            if isinstance(s2er, fnlist_cons):
                result = f0_s2exp(s2el.arg1, s2er.arg1, ctx)
                if isinstance(result, fnoptn_cons):
                    ctx = result.arg1
                    s2el, s2er = s2el.arg2, s2er.arg2
                else:
                    return fnoptn_nil()
            else:
                return fnoptn_nil()
        if isinstance(s2el, fnlist_nil) and isinstance(s2er, fnlist_nil):
            return fnoptn_cons(ctx)
        else:
            return fnoptn_nil()

    return f0_s2exp(s2el, s2er, CTXnil())

########################################################################

def s2exp_subst(s2el: s2exp, ctx: s2ctx) -> s2exp:
    """
    Replace variables in the left expression by their bindings in the context.
    Substitutions are simultaneous: a binding is not substituted again.
    The newest binding naming a variable wins; unbound variables are kept.
    Inputs are unchanged, and new nodes are built for every constructor.
    """
    def f0_s2var(s2ev: S2Evar) -> fnoptn[s2exp]:
        rest = ctx
        while isinstance(rest, CTXcns):
            if s2ev.arg1 == rest.arg1:
                if isinstance(rest.arg2, S2E000):
                    return fnoptn_cons(rest.arg2)
                else:
                    raise TypeError(
                        f"Unsupported binding: {type(rest.arg2).__name__}")
            else:
                rest = rest.arg3
        return fnoptn_nil()

    def f0_s2exp(s2el: s2exp) -> s2exp:
        if False:
            return S2E000()
        elif isinstance(s2el, S2Evar):
            found = f0_s2var(s2el)
            if isinstance(found, fnoptn_cons):
                s2er: s2exp = found.arg1
                return s2er
            else:
                return s2el
        elif isinstance(s2el, S2Econ):
            return S2Econ(s2el.arg1, f0_s2explst(s2el.arg2))
        elif isinstance(s2el, S2Efun):
            return S2Efun(f0_s2exp(s2el.arg1), f0_s2exp(s2el.arg2))
        elif isinstance(s2el, S2Etupl):
            return S2Etupl(f0_s2explst(s2el.arg1))
        elif type(s2el) is S2E000:
            return S2E000()
        raise TypeError(f"Unsupported static expression: {type(s2el).__name__}")

    def f0_s2explst(s2el: s2explst) -> s2explst:
        s2vs: list[s2exp] = []
        while isinstance(s2el, fnlist_cons):
            s2vs.append(f0_s2exp(s2el.arg1))
            s2el = s2el.arg2
        if not isinstance(s2el, fnlist_nil):
            raise TypeError(f"f0_s2explst({s2el})")
        result: s2explst = fnlist_nil()
        for s2v in reversed(s2vs):
            result = fnlist_cons(s2v, result)
        return result

    return f0_s2exp(s2el)

########################################################################

def s2ctx_merge(s2el: s2ctx, s2er: s2ctx) -> fnoptn[s2ctx]:
    """
    Combine two contexts as substitutions. A variable bound in both must
    have equal bindings, else fnoptn_nil. Otherwise the bindings are
    unioned, left context first; inputs are unchanged.
    """
    def f0_search(s2ev: strn, s2er: s2ctx) -> fnoptn[s2ctx]:
        rest = s2er
        while isinstance(rest, CTXcns):
            if s2ev == rest.arg1:
                return fnoptn_cons(rest)
            else:
                rest = rest.arg3
        return fnoptn_nil()

    def f0_agree(s2el: CTXcns, s2er: CTXcns) -> bool:
        if isinstance(s2el.arg2, S2E000) and isinstance(s2er.arg2, S2E000):
            return s2exp_equal(s2el.arg2, s2er.arg2)
        else:
            return s2el.arg2 == s2er.arg2

    def f0_check(s2el: s2ctx, s2er: s2ctx) -> bool:
        rest = s2el
        while isinstance(rest, CTXcns):
            found = f0_search(rest.arg1, s2er)
            if isinstance(found, fnoptn_cons):
                if not f0_agree(rest, found.arg1):
                    return False
            rest = rest.arg3
        return True

    if not f0_check(s2el, s2er):
        return fnoptn_nil()
    cells: list[CTXcns] = []
    rest = s2el
    while isinstance(rest, CTXcns):
        cells.append(rest)
        rest = rest.arg3
    rest = s2er
    while isinstance(rest, CTXcns):
        if isinstance(f0_search(rest.arg1, s2el), fnoptn_nil):
            cells.append(rest)
        rest = rest.arg3
    result: s2ctx = CTXnil()
    for cell in reversed(cells):
        result = CTXcns(cell.arg1, cell.arg2, result)
    return fnoptn_cons(result)

########################################################################

def s2ctx_fold_merge(acc: s2ctx, l: fnlist[s2ctx]) -> fnoptn[s2ctx]:
    rest = l
    while isinstance(rest, fnlist_cons):
        x = s2ctx_merge(acc, rest.arg1)
        if isinstance(x,fnoptn_cons):
            acc = x.arg1
            rest = rest.arg2
        else:
            return fnoptn()

    return fnoptn_cons(acc)

########################################################################

def d2exp_subst(dexp: d2exp, ctx: s2ctx) -> d2exp:
    """
    Rebuild an expression, applying the context to every static expression
    reachable from it: template argument types, implementation signatures,
    and the annotations of the parameters bound by lambdas and functions.
    Let expressions are walked, so their declarations are covered as well.
    Inputs are unchanged, and new nodes are built for every constructor.
    """
    def f0_d2eclist(decls: d2eclist) -> d2eclist:
        d2cs: list[d2ecl] = []
        while isinstance(decls, fnlist_cons):
            d2cs.append(f0_d2ecl(decls.arg1))
            decls = decls.arg2
        if not isinstance(decls, fnlist_nil):
            raise TypeError(f"f0_d2eclist({decls})")
        result: d2eclist = fnlist_nil()
        for d2c in reversed(d2cs):
            result = fnlist_cons(d2c, result)
        return result

    def f0_s2explst(s2el: s2explst) -> s2explst:
        s2vs: list[s2exp] = []
        while isinstance(s2el, fnlist_cons):
            s2vs.append(s2exp_subst(s2el.arg1, ctx))
            s2el = s2el.arg2
        if not isinstance(s2el, fnlist_nil):
            raise TypeError(f"f0_s2explst({s2el})")
        result: s2explst = fnlist_nil()
        for s2v in reversed(s2vs):
            result = fnlist_cons(s2v, result)
        return result

    def f0_d2expopt(body: d2expopt) -> d2expopt:
        if isinstance(body, fnoptn_cons):
            return fnoptn_cons(f0_d2exp(body.arg1))
        elif isinstance(body, fnoptn_nil):
            return fnoptn_nil()
        else:
            raise TypeError(f"f0_d2expopt({body})")

    def f0_d2explst(d2el: d2explst) -> d2explst:
        d2vs: list[d2exp] = []
        while isinstance(d2el, fnlist_cons):
            d2vs.append(f0_d2exp(d2el.arg1))
            d2el = d2el.arg2
        if not isinstance(d2el, fnlist_nil):
            raise TypeError(f"f0_d2explst({d2el})")
        result: d2explst = fnlist_nil()
        for d2v in reversed(d2vs):
            result = fnlist_cons(d2v, result)
        return result

    def f0_d2ecl(decl: d2ecl) -> d2ecl:
        if isinstance(decl, D2Cbind):
            return D2Cbind(decl.arg1, f0_d2exp(decl.arg2))
        elif isinstance(decl, D2Cimpl):
            return D2Cimpl(decl.arg1, f0_d2exp(decl.arg2), decl.arg3,
                           f0_s2explst(decl.arg4))
        elif isinstance(decl, D2Clocal):
            return D2Clocal(f0_d2eclist(decl.arg1), f0_d2eclist(decl.arg2))
        else:
            raise TypeError(f"d2exp_subst({decl})")

    def f0_d2exp(dexp: d2exp) -> d2exp:
        if False:
            return D2E000()
        elif isinstance(dexp, D2Eint):
            return dexp
        elif isinstance(dexp, D2Ebtf):
            return dexp
        elif isinstance(dexp, D2Estr):
            return dexp
        elif isinstance(dexp, D2Eop1):
            return D2Eop1(dexp.name, f0_d2exp(dexp.arg1))
        elif isinstance(dexp, D2Eop2):
            return D2Eop2(dexp.name,
                          f0_d2exp(dexp.arg1), f0_d2exp(dexp.arg2))
        elif isinstance(dexp, D2Evar):
            return dexp
        elif isinstance(dexp, D2Ecst):
            return dexp
        elif isinstance(dexp, D2Elam):
            return D2Elam(dexp.arg1, s2exp_subst(dexp.arg2, ctx),
                          f0_d2exp(dexp.arg3))
        elif isinstance(dexp, D2Efix):
            return D2Efix(dexp.arg1, dexp.arg2, s2exp_subst(dexp.arg3, ctx),
                          f0_d2exp(dexp.arg4), s2exp_subst(dexp.arg5, ctx))
        elif isinstance(dexp, D2Eapp):
            return D2Eapp(f0_d2exp(dexp.arg1), f0_d2exp(dexp.arg2))
        elif isinstance(dexp, D2Eif0):
            return D2Eif0(f0_d2exp(dexp.arg1),
                          f0_d2exp(dexp.arg2), f0_d2exp(dexp.arg3))
        elif isinstance(dexp, D2Etupl):
            return D2Etupl(f0_d2explst(dexp.arg1))
        elif isinstance(dexp, D2Eproj):
            return D2Eproj(dexp.arg1, f0_d2exp(dexp.arg2))
        elif isinstance(dexp, D2Elets):
            return D2Elets(f0_d2eclist(dexp.arg1), f0_d2expopt(dexp.arg2))
        elif isinstance(dexp, D2Etapp):
            return D2Etapp(f0_d2exp(dexp.arg1), f0_s2explst(dexp.arg2))
        elif type(dexp) is D2E000:
            return dexp
        raise TypeError(f"Unsupported level-2 expression: {type(dexp).__name__}")

    return f0_d2exp(dexp)
