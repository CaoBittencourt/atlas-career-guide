import python.base.type as types
from python.base import fam


# default method
def _ist(x, t: type):
    assert isinstance(t, type)
    return isinstance(x, t)


# the "is" family of functions
ist = fam(
    type=_ist,
    bool=types.boolean.bool,
    true=types.boolean.true,  # unnested for convenience
    false=types.boolean.false,  # unnested for convenience
    chr=types.chr,
    num=types.num,
    callable=types._callable,
    collection=types.collection,
    missing=types.missing.missing,
    null=types.missing.null,  # unnested for convenience
    na=types.missing.na,  # unnested for convenience
)
