# `families`: polymorphic families of functions (ironically, using classes)
This module defines the `fam` type, which is essentially a (potentially nested) named list of callable objects, usually functions. The `fam` object is itself callable, dispatching a default method (viz. the first on the list of methods provided). But it is also subsettable. Therefore, one can define polymorphic generic functions with default methods.

# Usage
To create a family of functions, one starts by writing its methods. These can, but do not have to, be defined for each data type. After this, just create a `fam` object with all methods.

```python
from python.base import fam
from numbers import Number


# default method
def _ist(x, t: type):
    assert isinstance(t, type)
    return isinstance(x, t)


# the "is" family of functions
ist = fam(
    type=_ist,
    null=lambda x: x is None,
    str=lambda x: isinstance(x, str),
    num=fam(
        any=lambda x: isinstance(x, Number),
        dbl=fam(
            any=lambda x: isinstance(x, float),
            pct=lambda x: all([isinstance(x, float), x >= 0, x <= 1]),
        ),
        int=lambda x: isinstance(x, int),
    ),
)
```

Access a family of functions' methods using dot notation:
```python
ist.null(None) # returns True
ist.null(19) # returns False

ist.str(19) # returns False
ist.str("19") # returns True

ist.num.dbl(19.0) # returns True
ist.num.dbl(19) # returns False
```

Or simply call it directly to dispatch the default method:
```python
ist(19, str) # returns False
ist("19", str) # returns True

ist.num(19) # returns True
ist.num(None) # returns False
```
