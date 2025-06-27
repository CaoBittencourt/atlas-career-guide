from python.base import fam
import python.base.type as types


# default method
def _ist(x, t: type):
    assert isinstance(t, type)
    return isinstance(x, t)


# the "is" family of functions
ist = fam(
    type=_ist,
    bool=types._bool,
    chr=types.chr,
    num=types.num,
    callable=types._callable,
    collection=types.collection,
    missing=types._missing,
    null=types._null,
    na=types._na,
)
