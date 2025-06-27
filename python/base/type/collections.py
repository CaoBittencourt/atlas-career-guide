from python.base.families import fam
from numpy.typing import ArrayLike
from pandas import DataFrame


# subtypes
def _df(x):
    return isinstance(x, DataFrame)


def _list(x):
    return isinstance(x, list)


def _tuple(x):
    return isinstance(x, tuple)


def _vector(x):
    return isinstance(x, ArrayLike)


def _fam(x):
    return isinstance(x, fam)


# default method
def _collection(x):
    return any(
        [
            _df(x),
            _list(x),
            _tuple(x),
            _vector(x),
            _fam(x),
        ]
    )


# collection type
collection = fam(
    collection=_collection,
    list=_list,
    vector=_vector,
    tuple=_tuple,
    df=_df,
    fam=_fam,
)
