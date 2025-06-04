# families of functions using classes

from numbers import Number


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
