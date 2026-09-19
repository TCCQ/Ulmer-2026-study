########################################################################
########################################################################
# HX-2026-09-19:
# For interpreting level-2 syntax of FWATS3.
# This is not template-resolution at level-2!
########################################################################
########################################################################
from abc import ABC
from \
dataclasses import dataclass
########################################################################
########################################################################
from FWATS3_2basics import *
########################################################################
########################################################################
@dataclass
class D2V000(ABC):
    ctag = "D2V000"
    pass
type d2val = D2V000
type d2valist = fnlist[d2val]
########################################################################
@dataclass\
(frozen=True)
class ENV000(ABC):
    ctag = "ENV000"
    pass
type d2env = ENV000
########################################################################
#
@dataclass\
(frozen=True)
class ENVnil(ENV000):
    pass
#
@dataclass\
(frozen=True)
class ENVcns(ENV000):
    arg1: d2var
    arg2: d2val
    arg3: d2env
    ctag = "ENVcns"
#
########################################################################
def d2env_search(denv: d2env, dvar: d2var) -> d2val:
    while True:
        if isinstance(denv, ENVcns):
            if dvar == denv.arg1:
                return denv.arg2
            else:
                denv = denv.arg3; continue
        else:
            return D2V000() # HX: this indicates an error
    # end-of-(while True)
########################################################################
@dataclass
class D2Vint(D2V000):
    arg1: sint
    ctag = "D2Vint"
########################################################################
@dataclass
class D2Vbtf(D2V000):
    arg1: bool
    ctag = "D2Vbtf"
########################################################################
@dataclass
class D2Vstr(D2V000):
    arg1: strn
    ctag = "D2Vstr"
########################################################################
@dataclass
class D2Vtupl(D2V000):
    arg1: d2valist
    ctag = "D2Vtupl"
########################################################################
def D2Vnil() -> D2Vtupl:
    return D2Vtupl(fnlist_nil())
########################################################################
@dataclass
class D2Vlam(D2V000):
    arg1: d2env
    arg2: D2Elam
    ctag = "D2Vlam"
########################################################################
@dataclass
class D2Vfix(D2V000):
    arg1: d2env
    arg2: D2Efix
    ctag = "D2Vfix"
########################################################################
########################################################################
#
def d2exp_evaluate\
(dexp: d2exp, denv: d2env = ENVnil()) -> d2val:
    """
    HX: [dexp] may contain free variables
    """
######
    def f0_D2Eif0(dexp: D2Eif0) -> d2val:
        dexp1 = d2exp_evaluate(dexp.arg1, denv)
        assert isinstance(dexp1, D2Vbtf)
        if dexp1.arg1:
            return d2exp_evaluate(dexp.arg2, denv)
        else:
            return d2exp_evaluate(dexp.arg3, denv)
######
    def f0_D2Eop1(dexp: D2Eop1) -> d2val:
        name = dexp.name
        dexp1 = d2exp_evaluate(dexp.arg1, denv)
        match name:
            case "+1":
                assert isinstance(dexp1, D2Vint)
                return D2Vint(dexp1.arg1+1)
            case "-1":
                assert isinstance(dexp1, D2Vint)
                return D2Vint(dexp1.arg1-1)
            case _:
                raise TypeError(f"f0_D2Eop1({dexp})")
######
    def f0_D2Eop2(dexp: D2Eop2) -> d2val:
        name = dexp.name
        dexp1 = d2exp_evaluate(dexp.arg1, denv)
        dexp2 = d2exp_evaluate(dexp.arg2, denv)
        match name:
            case "+":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vint(dexp1.arg1+dexp2.arg1)
            case "-":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vint(dexp1.arg1-dexp2.arg1)
            case "*":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vint(dexp1.arg1*dexp2.arg1)
            case "<":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vbtf(dexp1.arg1<dexp2.arg1)
            case ">":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vbtf(dexp1.arg1>dexp2.arg1)
            case "<=":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vbtf(dexp1.arg1<=dexp2.arg1)
            case ">=":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vbtf(dexp1.arg1>=dexp2.arg1)
            case "==":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vbtf(dexp1.arg1==dexp2.arg1)
            case "!=":
                assert isinstance(dexp1, D2Vint)
                assert isinstance(dexp2, D2Vint)
                return D2Vbtf(dexp1.arg1!=dexp2.arg1)
            case _:
                raise TypeError(f"f0_D2Eop2({dexp})")
######
    def f0_D2Evar(dexp: D2Evar) -> d2val:
        return d2env_search(denv, dexp.arg1)
######
    def f0_D2Eapp(dexp: D2Eapp) -> d2val:
        dfun = d2exp_evaluate(dexp.arg1, denv)
        darg = d2exp_evaluate(dexp.arg2, denv)
        if isinstance(dfun, D2Vlam):
            dlam = dfun.arg2
            denv_new = \
                ENVcns(dlam.arg1, darg, dfun.arg1)
            return d2exp_evaluate(dlam.arg3, denv_new)
        elif isinstance(dfun, D2Vfix):
            dfix = dfun.arg2
            denv_new0 = dfun.arg1
            denv_new1 = \
                ENVcns(dfix.arg1, dfun, denv_new0)
            denv_new2 = \
                ENVcns(dfix.arg2, darg, denv_new1)
            return d2exp_evaluate(dfix.arg4, denv_new2)
        else:
            raise TypeError(f"f0_D2Eapp({dexp})")
######    
    def f0_D2Etupl(dexp: D2Etupl) -> d2val:
        d2es = dexp.arg1
        d2vs: list[d2val] = []
        while isinstance(d2es, fnlist_cons):
            d2vs.append(d2exp_evaluate(d2es.arg1, denv))
            d2es = d2es.arg2
        if not isinstance(d2es, fnlist_nil):
            raise TypeError(f"f0_D2Etupl({dexp})")
        result: d2valist = fnlist_nil()
        for dval in reversed(d2vs):
            result = fnlist_cons(dval, result)
        return D2Vtupl(result)
######
    def f0_D2Elets(dexp: D2Elets) -> d2val:
        denv_new = d2eclist_evaluate(dexp.arg1, denv)
        body = dexp.arg2
        if isinstance(body, fnoptn_cons):
            return d2exp_evaluate(body.arg1, denv_new)
        elif isinstance(body, fnoptn_nil):
            return D2Vnil()
        else:
            raise TypeError(f"f0_D2Elets({dexp})")
######
    if False:
        return D2V000()
    elif isinstance(dexp, D2Eint):
        return D2Vint(dexp.arg1)
    elif isinstance(dexp, D2Ebtf):
        return D2Vbtf(dexp.arg1)
    elif isinstance(dexp, D2Estr):
        return D2Vstr(dexp.arg1)
    elif isinstance(dexp, D2Elam):
        return D2Vlam(denv, dexp)
    elif isinstance(dexp, D2Efix):
        return D2Vfix(denv, dexp)
    elif isinstance(dexp, D2Eif0): return f0_D2Eif0(dexp)
    elif isinstance(dexp, D2Eop1): return f0_D2Eop1(dexp)
    elif isinstance(dexp, D2Eop2): return f0_D2Eop2(dexp)
    elif isinstance(dexp, D2Evar): return f0_D2Evar(dexp)
    elif isinstance(dexp, D2Eapp): return f0_D2Eapp(dexp)
    elif isinstance(dexp, D2Elets): return f0_D2Elets(dexp)
    elif isinstance(dexp, D2Etupl): return f0_D2Etupl(dexp)
    else:
        raise TypeError(f"d2exp_evaluate({dexp})")
#
########################################################################
########################################################################
def d2ecl_evaluate(decl: d2ecl, denv: d2env) -> d2env:
    """
    Evaluate a binding or local declaration without changing [denv].
    """
    if isinstance(decl, D2Cbind):
        return ENVcns(decl.arg1, d2exp_evaluate(decl.arg2, denv), denv)
    elif isinstance(decl, D2Clocal):
        denv_head = d2eclist_evaluate(decl.arg1, denv)
        denv_body = d2eclist_evaluate(decl.arg2, denv_head)
        # Copy only the public extension; closures retain their captured scope.
        bindings: list[tuple[d2var, d2val]] = []
        while denv_body is not denv_head:
            if not isinstance(denv_body, ENVcns):
                raise TypeError(f"d2ecl_evaluate({decl})")
            bindings.append((denv_body.arg1, denv_body.arg2))
            denv_body = denv_body.arg3
        for name, dval in reversed(bindings):
            denv = ENVcns(name, dval, denv)
        return denv
    else:
        raise TypeError(f"d2ecl_evaluate({decl})")
########################################################################
def d2eclist_evaluate(decls: d2eclist, denv: d2env) -> d2env:
    """
    Evaluate declarations in order, extending the environment at each step.
    """
    while isinstance(decls, fnlist_cons):
        denv = d2ecl_evaluate(decls.arg1, denv)
        decls = decls.arg2
    if not isinstance(decls, fnlist_nil):
        raise TypeError(f"d2eclist_evaluate({decls})")
    return denv
########################################################################
# end of
# [Ulmer-2026-study/contrib/githwxi/FWATS3/srcgen1/FWATS3_2interp.py]
########################################################################
########################################################################
