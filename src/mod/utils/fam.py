# families of functions using classes

from numbers import Number


class Is:
    def __new__(cls, x):
        return not Is.null(x)

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


Is("dsds")
Is(None)

Is.null("dsds")
Is.null(None)

Is.double(None)
Is.double("dsds")
Is.double(19)
Is.double(19.0)

Is.integer(None)
Is.integer("dsds")
Is.integer(19)
Is.integer(19.0)

Is.numeric(None)
Is.numeric("dsds")
Is.numeric(19)
Is.numeric(19.0)
