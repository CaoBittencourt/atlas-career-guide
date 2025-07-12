# `type`: functional type definitions
This directory defines data types with polymorphic generic functions. For instance,

```python
# the numeric type
num = fam(
    num=_num, # default method (any numeric type)
    int=fam( # integer subtype
        int=_int, # default integer method
        bin=_bin, # binary integers (0 or 1)
    ),
    dbl=fam( # double (i.e. floats) subtype
        dbl=_dbl, # default double method
        pct=fam( # percentage subtype (doubles between 0 and 1)
            pct=_pct, # default percentage method
            prop=_prop, # proportions (percentages that sum to 1)
        ),
    ),
)
```
