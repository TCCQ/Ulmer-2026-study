########################################################################
########################################################################
# HX-2026-09-18:
# This is level-2 syntax for FWATS3.
# Not decision yet as to whether concrete syntax (level-0) is needed.
########################################################################
########################################################################
from abc import ABC
from enum import Enum
from dataclasses import dataclass
from typing import \
    Generic, TypeVar, Callable
########################################################################
########################################################################
#
# python3.12
#
type nint = int
type sint = int
type strn = str
########################################################################
type s2var = strn
########################################################################
type d2var = strn
type d2cst = strn
########################################################################
########################################################################
T = TypeVar("T")
X = TypeVar("X")
Y = TypeVar("Y")
########################################################################
########################################################################
#
@dataclass
class fnoptn[T](ABC):
    pass
@dataclass
class fnoptn_nil[T](fnoptn[T]):
    pass
@dataclass
class fnoptn_cons[T](fnoptn[T]):
    arg1: T
    pass
#
########################################################################
#
@dataclass
class fnlist[T](ABC):
    pass
@dataclass
class fnlist_nil[T](fnlist[T]):
    pass
@dataclass
class fnlist_cons[T](fnlist[T]):
    arg1: T
    arg2: fnlist[T]
    pass
#
########################################################################
########################################################################
@dataclass
class S2E000(ABC):
    ctag = "S2E000"
    pass
type s2varlst = fnlist[s2var]
type s2exp = S2E000
type s2explst = fnlist[s2exp]
########################################################################
########################################################################
@dataclass
class S2Econ(S2E000):
    arg1: strn
    arg2: s2explst
    ctag = "S2Econ"
########################################################################
@dataclass
class S2Evar(S2E000):
    arg1: s2var
    ctag = "S2Evar"
########################################################################
@dataclass
class S2Efun(S2E000):
    arg1: s2exp
    arg2: s2exp
    ctag = "S2Efun"
########################################################################
@dataclass
class S2Etupl(S2E000):
    arg1: s2explst
    ctag = "S2Etupl"
########################################################################
########################################################################
@dataclass
class D2E000(ABC):
    ctag = "D2E000"
    pass
type d2exp = D2E000
type d2expopt = fnoptn[d2exp]
type d2explst = fnlist[d2exp]
########################################################################
########################################################################
@dataclass
class D2C000(ABC):
    ctag = "D2C000"
    pass
type d2ecl = D2C000
type d2eclist = fnlist[d2ecl]
########################################################################
########################################################################
@dataclass
class D2Eint(D2E000):
    arg1: sint
    ctag = "D2Eint"
########################################################################
@dataclass
class D2Ebtf(D2E000):
    arg1: bool
    ctag = "D2Ebtf"
########################################################################
@dataclass
class D2Estr(D2E000):
    arg1: strn
    ctag = "D2Estr"
########################################################################
@dataclass
class D2Eop1(D2E000):
    name: strn
    arg1: d2exp
    ctag = "D2Eop1"
########################################################################
@dataclass
class D2Eop2(D2E000):
    name: strn
    arg1: d2exp
    arg2: d2exp
    ctag = "D2Eop2"
########################################################################
@dataclass
class D2Evar(D2E000):
    arg1: d2var
    ctag = "D2Evar"
########################################################################
@dataclass
class D2Ecst(D2E000):
    arg1: d2cst
    ctag = "D2Ecst"
########################################################################
#
# lam x. body(x)
# fix f(x). body(f,x)
# 
@dataclass
class D2Elam(D2E000):
    arg1: d2var
    arg2: s2exp
    arg3: d2exp
    ctag = "D2Elam"
#
@dataclass
class D2Efix(D2E000):
    arg1: d2var
    arg2: d2var
    arg3: s2exp
    arg4: d2exp
    arg5: s2exp
    ctag = "D2Efix"
#
########################################################################
@dataclass
class D2Eapp(D2E000):
    arg1: d2exp
    arg2: d2exp
    ctag = "D2Eapp"
########################################################################
@dataclass
class D2Eif0(D2E000):
    arg1: d2exp
    arg2: d2exp
    arg3: d2exp
    ctag = "D2Eif0"
########################################################################
@dataclass
class D2Etupl(D2E000):
    arg1: d2explst
    ctag = "D2Etupl"
########################################################################
@dataclass
class D2Eproj(D2E000):
    arg1: nint
    arg2: d2exp
    ctag = "D2Eproj"
########################################################################
@dataclass
class D2Elets(D2E000):
    arg1: d2eclist
    arg2: d2expopt
    ctag = "D2Elets"
########################################################################
#
# HX-2026-09-19:
# [arg1] is template name
# [arg2] is template params
#
@dataclass
class D2Etapp(D2E000):
    arg1: d2exp
    arg2: s2explst
    ctag = "D2Etapp"
########################################################################
########################################################################
@dataclass
class D2Cbind(D2C000):
    arg1: d2var
    arg2: d2exp
    ctag = "D2Cbind"
########################################################################
@dataclass
class D2Cimpl(D2C000):
    arg1: d2cst
    arg2: d2exp
    arg3: s2varlst
    arg4: s2explst
    ctag = "D2Cimpl"
########################################################################
@dataclass
class D2Clocal(D2C000):
    arg1: d2eclist
    arg2: d2eclist
    ctag = "D2Clocal"
########################################################################
########################################################################
# end of
# [Ulmer-2026-study/contrib/githwxi/FWATS3/srcgen1/FWATS3_2basics.py]
########################################################################
########################################################################
