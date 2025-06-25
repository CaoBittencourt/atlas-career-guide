from python.base import fam
import python.base.type as types


# default method
def _ist(x, t: type):
    assert isinstance(t, type)

    return isinstance(x, t)


# the "is" family of functions
ist = fam(
    type=_ist,
    null=types._null,
    chr=types.chr,
    num=types.num,
)
