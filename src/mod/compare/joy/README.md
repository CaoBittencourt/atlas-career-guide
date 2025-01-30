# `joy`
This module is designed for utility-based job satisfaction estimations.

## `ubar` aggregate utility methods
More precisely, given a utility function
```math
u(\upsilon_k, a_q) : [0,1]^{2m} \rightarrow [0,1]^{m},
```
which measures an individual $k$'s satisfaction when working with skill set 
```math
a_q := (a_{1}^{q}, \dots, a_{m}^{q}) \in [0,1] ^ m,
```
based on their preferences vector
```math
\upsilon_k := (\upsilon_{1}^{k}, \dots, \upsilon_{m}^{k}) \in [0,1] ^ m,
```
we can write aggregate job satisfaction as
```math
\bar{u}_{kq} :=
\sum_{i=1}^{m}{
    \ddot{\upsilon}_{i}^{k} \times
    u(\upsilon_{i}^{k}, a_{i}^{q})
}
\in [0,1]
,
```
where
```math
\ddot{\upsilon}_{i}^{k} :=
\text{ueq}(\upsilon_k)
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
\begin{gather*}
\bar{u}_{kq} :=
\left(
    \prod_{i=1}^{m}{
        u(\upsilon_{i}^{k}, a_{i}^{q}) ^ {
            \ddot{\upsilon}_{i}^{k}
        }
    }
\right) ^ {
    \frac{
        1
    }{
        \sum_{i=1}^{m}{
            \ddot{\upsilon}_{i}^{k}
        }
    }
}
\in [0,1]
,
\\
\bar{u}_{kq} :=
\prod_{i=1}^{m}{
    u(\upsilon_{i}^{k}, a_{i}^{q}) ^ {
        \frac{
            \ddot{\upsilon}_{i}^{k}
        }{
            \sum_{i=1}^{m}{
                \ddot{\upsilon}_{i}^{k}
            }
        }
    }
}
\in [0,1]
,
\end{gather*}
```
respectively. Note the choice of aggregation function implies certain psychological hypotheses about workers' job satisfaction (e.g. attributes are complimentary if the function is concave, but substitutes if it is linear). 

## `utility` methods
Note also the utility function $u(\upsilon_k, a_q)$ does not have to be monotonically increasing, allowing for saturation effects, such that an individual's well-being can decrease when their job requires a higher skill level than they wished to employ. The simplest example would be
```math
u(\upsilon_{i}^{k}, a_{i}^{q}) :=
1-(2a_{i}^{q})^{2}
\in
[0,1]
\
\forall
\
i \in \{1, \dots, m\},
```
but many other methods can be defined.