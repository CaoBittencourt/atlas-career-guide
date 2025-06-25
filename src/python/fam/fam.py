# families of functions using classes

from numbers import Number
from python.utils import ignore_unmatched_kwargs


class fam:
    # define static methods
    def __new__(cls, **kwargs):
        inst = super().__new__(cls)
        for k, v in kwargs.items():
            setattr(inst, k, v)
        return inst

    # return default method
    def __call__(self, *args, **kwargs):
        return ignore_unmatched_kwargs(next(iter(self.__dict__.values())))(args, kwargs)

    # don't instantiate the class
    def __init__(self, **kwargs):
        pass


ist = fam(
    any=lambda x: x is not None,
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
ist(19)
ist.num.dbl.pct(19)
ist.num.dbl.pct(1.0)
ist.num.dbl.pct(0.0)
ist.num.dbl.pct(-0.1)

ist.num()(19)
ist.num.dbl(19.0)
ist.num()(19)
ist.num()("19")
ist.num()(19.0)
ist.num()("19.0")
ist.num.int(19)
ist.num.num(19)
ist()(19)
ist.dbl(19)
ist.dbl(19.0)
ist.dbl("19.0")
ist.int(19)
ist.int(19.0)
ist.int("19")
ist.str(19)
ist.str("19")
ist.null(19)
ist.null(None)
