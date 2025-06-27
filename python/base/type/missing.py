from python.base.families import fam
from math import isnan


def _null(x):
    return x is None


def _na(x):
    return False if x is None else isnan(x)


def _missing(x):
    return any([_null(x), _na(x)])


missing = fam(
    missing=_missing,
    null=_null,
    na=_na,
)
