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


dsds = fam(
    null=lambda x: x is None,
    numeric=(lambda x: isinstance(x, Number)),
)


dsds()(19)
dsds()
dsds()
dsds
dsds(19)
dsds("19")
dsds(None)
dsds.numeric(19)
dsds.numeric("19")


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
