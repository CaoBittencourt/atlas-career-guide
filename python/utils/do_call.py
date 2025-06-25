from types import FunctionType
from inspect import Parameter, signature


def _contains_var_kwarg(f: FunctionType):
    return any(
        param.kind == Parameter.VAR_KEYWORD
        for param in signature(f).parameters.values()
    )


def do_call(f: FunctionType, *args, **kwargs):
    # if _contains_var_kwarg(f) else f(args)
    return f(args, kwargs)


def dsds(x):
    return x


def lalala(*args, **kwargs):
    return kwargs.items()


(do_call(dsds, x=19))
(do_call(lalala, "dsds", x=19, a=19))
