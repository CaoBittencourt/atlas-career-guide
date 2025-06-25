from python.base import fam
from numbers import Number


# default method
def _num(x):
    return isinstance(x, Number)


# subtypes
def _int(x):
    return isinstance(x, int)


def _bin(x):
    return x == 1 or x == 0


def _dbl(x):
    return isinstance(x, float)


def _pct(x):
    return all([_dbl(x), x >= 0, x <= 1])


def _prop(x):
    return all([_pct(x), sum(x) == 1])


# numeric family type definitions
num = fam(
    num=_num,
    int=fam(
        int=_int,
        bin=_bin,
    ),
    dbl=fam(
        dbl=_dbl,
        pct=fam(
            pct=_pct,
            prop=_prop,
        ),
    ),
)
