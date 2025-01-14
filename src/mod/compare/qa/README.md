# `qa`
Qualification (`qa`) models measure to which extent an individual is qualified relative to an occupation. As opposed to other comparative models, qualification, by default, is unweighted. The reason for this is that, here, we're not necessarily interested in matching individuals to occupations, but in assessing how much of occupations' attribute requirements they can fulfill. More precisely, we count here, exactly, how much of their skill set is greater or equal to the required skill levels. We, then, normalize the sum of scores to the unit interval to provide an estimation of the percentage of required skill levels they already have.

Mathematically, given the gap function
```math
\begin{gather}
    \delta(a_{i}^{k}, a_{i}^{q}) :=
    \max(
    a_{i}^{k} - a_{i}^{q}
    , 0
    )
    \in [0, 1]
    ,
\end{gather}
```
which measures only positive competency gaps (so as to not "punish" overqualification), we can write three unweighted qualification coefficients:
```math
\begin{align}
\tilde{\delta}_{kq}^{\geq}
&:=
\text{oqa}(
    \boldsymbol{a_k},
    \boldsymbol{a_q}
)
:=
\frac{
    \sum_{i=1}^{m}{
        \delta(a_{i}^{k}, a_{i}^{q})
    }
}{
    \sum_{i=1}^{m}{
        \delta(1, a_{i}^{q})
    }
}
=
\frac{
    \sum_{i=1}^{m}{
        \delta(a_{i}^{k}, a_{i}^{q})
    }
}{
    \sum_{i=1}^{m}{
        (1 - a_{i}^{q})
    }
}
;\\
\end{align}
```
$$
\begin{align}
    ;\\
    s_{kq}^{\geq}
    &:=
    \text{sqa}(\boldsymbol{a_k}, \boldsymbol{a_q}) :=
    1 -
    \frac{
    \sum_{i=1}^{m}{
    {\delta(a_{i}^{q}, a_{i}^{k})}
    }
    }{
    \sum_{i=1}^{m}{
    {\delta(a_{i}^{q}, 0)}
    }
    } =
    1 - \text{uqa}(\boldsymbol{a_k}, \boldsymbol{a_q})
    ;\\
    \tilde{\delta}_{kq}^{<}
    &:=
    \text{uqa}(\boldsymbol{a_k}, \boldsymbol{a_q}) :=
    \frac{
    \sum_{i=1}^{m}{
    {\delta(a_{i}^{q}, a_{i}^{k})}
    }
    }{
    \sum_{i=1}^{m}{
    {\delta(a_{i}^{q}, 0)}
    }
    } =
    \frac{
    \sum_{i=1}^{m}{
    {\delta(a_{i}^{q}, a_{i}^{k})}
    }
    }{
    \sum_{i=1}^{m}{
    {a_{i}^{q}}
    }
    }
    .
\end{align}
$$

The first of these is the overqualification coefficient and measures the percentage of a skill set in which an individual is overqualified. The second coefficient is that of "sufficient qualification" and measures the percentage of a skill set in which an individual is not underqualified. And lastly, the underqualification coefficient measures the percentage of a skill set which is higher than an individuals' current capacity.

Note, again, normalized (non-negative) Euclidean distance is the only method of estimation. This is because the concept of qualification itself determines we use actual attribute gaps instead of similarity metrics.

And note, also, all three coefficients are unweighted. Therefore, they shouldn't be used in matching, for they do not account for each attribute's importance to each occupation. If one wished to apply the qualification coefficient for matching, the following formulae, with attribute equivalence weights, would be more appropriate:
$$
\begin{align}
    \ddot{\delta}_{kq}^{\geq} 
    &:=
    \frac{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(a_{i}^{k}, a_{i}^{q})}
    }
    }{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(1, a_{i}^{q})}
    }
    } =
    \frac{
    \sum_{i=1}^{m}{
    {\delta(a_{i}^{k}, a_{i}^{q})}
    }
    }{
    \sum_{i=1}^{m}{
    {(1 - a_{i}^{q})}
    }
    }
    ;\\
    \ddot{s}_{kq}^{\geq}
    &:=
    1 -
    \frac{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(a_{i}^{q}, a_{i}^{k})}
    }
    }{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(a_{i}^{q}, 0)}
    }
    } =
    1 - \text{uqa}(\boldsymbol{a_k}, \boldsymbol{a_q})
    ;\\
    \ddot{\delta}_{kq}^{<}
    &:=
    \text{uqa}(\boldsymbol{a_k}, \boldsymbol{a_q}) :=
    \frac{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(a_{i}^{q}, a_{i}^{k})}
    }
    }{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(a_{i}^{q}, 0)}
    }
    } =
    \frac{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {\delta(a_{i}^{q}, a_{i}^{k})}
    }
    }{
    \sum_{i=1}^{m}{
    \ddot{a}_{i}^{q}
    {a_{i}^{q}}
    }
    }
    .
\end{align}
$$