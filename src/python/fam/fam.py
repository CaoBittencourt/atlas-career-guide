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
    string=lambda x: isinstance(x, str),
    double=lambda x: isinstance(x, float),
    integer=lambda x: isinstance(x, int),
    numeric=lambda x: isinstance(x, Number),
)

ist()(19)
ist()(None)


class ist:
    def __new__(cls, x):
        return not ist.null(x)

    def __init__():
        pass

    @staticmethod
    def null(x):
        return x is None

    @staticmethod
    def string(x):
        return isinstance(x, str)

    @staticmethod
    def double(x):
        return isinstance(x, float)

    @staticmethod
    def integer(x):
        return isinstance(x, int)

    @staticmethod
    def numeric(x):
        return isinstance(x, Number)


ist("dsds")
ist(None)

ist.null("dsds")
ist.null(None)

ist.double(None)
ist.double("dsds")
ist.double(19)
ist.double(19.0)

ist.integer(None)
ist.integer("dsds")
ist.integer(19)
ist.integer(19.0)

ist.numeric(None)
ist.numeric("dsds")
ist.numeric(19)
ist.numeric(19.0)
