# families of functions using classes

from numbers import Number


class fam:
    # define static methods
    def __new__(cls, **kwargs):
        inst = super().__new__(cls)
        for k, v in kwargs.items():
            setattr(inst, k, v)
        return inst

    # return default method
    def __call__(self, *args, **kwargs):
        return next(iter(self.__dict__.values()))

    # don't instantiate the class
    def __init__(self, **kwargs):
        pass


ist = fam(
    any=lambda x: x is not None,
    null=lambda x: x is None,
    str=lambda x: isinstance(x, str),
    dbl=lambda x: isinstance(x, float),
    int=lambda x: isinstance(x, int),
    num=lambda x: isinstance(x, Number),
)

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
