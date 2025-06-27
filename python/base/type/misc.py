from math import isnan


def _null(x):
    return x is None


def _na(x):
    return isnan(x)
