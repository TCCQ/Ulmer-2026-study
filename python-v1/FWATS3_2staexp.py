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
    strn, s2exp, s2explst, fnlist_cons, fnlist_nil,
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
    arg2: s2exp
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
                    if s2exp_equal(rest.arg2, s2er):
                        return fnoptn_cons(ctx)
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
