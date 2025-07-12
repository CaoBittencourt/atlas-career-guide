from python.base.families import fam


def _bool(x):
    return isinstance(x, bool)


def _true(x):
    return x is True


def _false(x):
    return x is False


boolean = fam(
    bool=_bool,
    true=_true,
    false=_false,
)
