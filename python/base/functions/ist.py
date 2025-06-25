from python.base import fam
from numbers import Number


# default method
def _ist(x, t: type):
    assert isinstance(t, type)

    return isinstance(x, t)


# the "is" family of functions
ist = fam(
    type=_ist,
    null=lambda x: x is None,
    str=lambda x: isinstance(x, str),
    num=fam(
        any=lambda x: isinstance(x, Number),
        dbl=fam(
            any=lambda x: isinstance(x, float),
            pct=lambda x: all([isinstance(x, float), x >= 0, x <= 1]),
        ),
        int=lambda x: isinstance(x, int),
    ),
)
