########################################################################

"""
Template resolution on level-2 static expressions.
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
    strn, s2exp, s2explst, fnlist, fnlist_cons, fnlist_nil,
    d2exp, d2expopt, d2explst, d2ecl, d2eclist,
    fnoptn, fnoptn_cons, fnoptn_nil,
)

from FWATS3_2staexp import (
    s2ctx, CTXnil, CTXcns, s2exp_equal, s2explst_equal, 
    s2exp_match, s2exp_subst
)

"""
The plan is to:

traverse the top level from top to bottom:
  if decl is Timpl, add name + arg types to ctx
  if decl is expr or regular decl:
    traverse expr or rhs, if we find Tuse:
      scan ctx backwards for timp where name matches and all args
        match in the s2exp_match sense.
      splice body of matching Timp with arguments substituted
      continue recursively traversing resulting expr
"""

type s2subst = fnlist[tuple[S2Evar, s2exp]]


def s2tmp_collect(e: d2ecl, ctx: s2ctx) -> fnoptn[s2ctx]:
    if False:
        return fnoptn_nil()
    elif isinstance(e, D2Cimpl):
        return fnoptn_cons(CTXcns(e.arg1, e, ctx))
    else:
        return fnoptn_cons(ctx)

def s2tmp_choose_impl(tuse: D2Etapp, ctx: s2ctx) -> fnoptn[d2exp]:
    name = ""
    if isinstance(tuse.arg1, D2Ecst):
        name = tuse.arg1.arg1
    else:
        return fnoptn_nil
    rest = ctx
    while isinstance(rest, fn_list_cons):
        if rest.arg1 == name and isinstance(rest.arg2, D2impl):
            tibody = rest.arg2.arg2
            tivars = rest.arg2.arg3 # do we need this?
            tiargs = rest.arg2.arg4

            tapp_args = e.arg2
            # want zipWith s2exp_match tiargs tapp_args
            # then sequence options
            # then merge substitutions, checking for conflicts
            # then apply to body and return
            
# SNIP -------------------------------

def d2exp_no_tapp(dexp: d2exp, ctx: s2ctx) -> fnoptn[d2exp]:
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

    def f0_d2ecl(decl: d2ecl) -> d2ecl:
        if isinstance(decl, D2Cbind):
            return D2Cbind(decl.arg1, f0_d2exp(decl.arg2))
        elif isinstance(decl, D2Cimpl):
            return D2Cimpl(decl.arg1, f0_d2exp(decl.arg2), decl.arg3, decl.arg4)
        elif isinstance(decl, D2Clocal):
            return D2Clocal(f0_d2eclist(decl.arg1), f0_d2eclist(decl.arg2))
        else:
            raise TypeError(f"d2exp_no_tapp({decl})")

    def f0_d2expopt(body: d2expopt) -> d2expopt:
        if isinstance(body, fnoptn_cons):
            return fnoptn_cons(f0_d2exp(body.arg1))
        elif isinstance(body, fnoptn_nil):
            return fnoptn_nil()
        else:
            raise TypeError(f"f0_d2expopt({body})")

    def f0_d2explst(d2es: d2explst) -> d2explst:
        d2vs: list[d2exp] = []
        while isinstance(d2es, fnlist_cons):
            d2vs.append(f0_d2exp(d2es.arg1))
            d2es = d2es.arg2
        if not isinstance(d2es, fnlist_nil):
            raise TypeError(f"f0_d2explst({d2es})")
        result: d2explst = fnlist_nil()
        for d2v in reversed(d2vs):
            result = fnlist_cons(d2v, result)
        return result

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
            return D2Elam(dexp.arg1, dexp.arg2, f0_d2exp(dexp.arg3))
        elif isinstance(dexp, D2Efix):
            return D2Efix(dexp.arg1, dexp.arg2, dexp.arg3,
                          f0_d2exp(dexp.arg4), dexp.arg5)
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
            raise TypeError("TODO replace with substitute body from context or fail")
        elif type(dexp) is D2E000:
            return dexp
        raise TypeError(f"Unsupported level-2 expression: {type(dexp).__name__}")p

    result = f0_d2exp(dexp)
    if type(result) is D2E000:
        return fnoptn_nil()
    else:
        return fnoptn_cons(result)

