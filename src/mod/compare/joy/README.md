# `joy`
This module is designed for utility-based job satisfaction estimations.

## `ubar` aggregate utility methods
More precisely, given a utility function
```math
u(\upsilon_k, a_q) : [0,1]^{2m} \rightarrow [0,1]^{m},
```
which measures an individual $k$'s satisfaction when working with skill set 
```math
a_q := (a_{q}^{1}, \dots, a_{q}^{m}) \in [0,1] ^ m,
```
based on their preferences vector
```math
\upsilon_k := (\upsilon_{k}^{1}, \dots, \upsilon_{k}^{m}) \in [0,1] ^ m,
```
we can write aggregate job satisfaction as
```math
\bar{u}_{kq} :=
\sum_{i=1}^{m}{
    \ddot{\upsilon}_{k}^{i} \times
    u(\upsilon_{k}^{i}, a_{q}^{i})
}
\in [0,1]
,
```
where
```math
\ddot{\upsilon}_{k}^{i} :=
\text{ueq}(\upsilon_{k}^{i}, \upsilon_k)
\in
[0,1]
\
\forall
\
i \in \{1, \dots, m\}
```
is "equivalent utility" and quantifies the subjective importance of attribute $i$ to person $k$ relative to their most preferred attribute.

Or, alternatively, with geometric mean or Cobb-Douglas aggregation functions,
```math
\bar{u}_{kq} :=
\left(
    \prod_{i=1}^{m}{
        u(\upsilon_{k}^{i}, a_{q}^{i}) ^ {
            \ddot{\upsilon}_{k}^{i}
        }
    }
\right) ^ {
    \frac{
        1
    }{
        \sum_{i=1}^{m}{
            \ddot{\upsilon}_{k}^{i}
        }
    }
}
\in [0,1]
,
\\
\bar{u}_{kq} :=
\prod_{i=1}^{m}{
    u(\upsilon_{k}^{i}, a_{q}^{i}) ^ {
        \frac{
            \ddot{\upsilon}_{k}^{i}
        }{
            \sum_{i=1}^{m}{
                \ddot{\upsilon}_{k}^{i}
            }
        }
    }
}
\in [0,1]
,
```
respectively.

## `utility` methods
Note the utility function $u(\upsilon_k, a_q)$ does not have to be monotonically increasing, allowing for saturation effects, such that an individual's well-being can decrease when their job requires a higher skill level than they wished to employ. The simplest example would be
```math
u(\upsilon_{k}^{i}, a_{q}^{i}) :=
-(2a_{q}^{i}-\upsilon_{k}^{i})^{2}+1
\in
[0,1]
\
\forall
\
i \in \{1, \dots, m\},
```
but many other methods can be defined.