from python.utils import ignore_unmatched_kwargs


# families of functions using classes
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
    def __init__(self, *args, **kwargs):
        pass
