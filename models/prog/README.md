# `models/prog`
## Unidirectional Utility-Maximizing Career Progression Model
- original multidirectional model was prohibitively slow
- simplifying assumptions of the unidirectional model:
    - only one optimal career path (based on an ordering variable, aka objective function)
    - no significant barriers to career progression
        - i.e. individuals are guaranteed to find at least one job in any occupation where they are employable
        - this implies the labor market is assumed to be "static" from the point of view of a single worker
        - therefore, one has only one optimal career progression
- mathematically,

```math
\argmax_{q}\left(
    [u(y_k,y_k, \dots) \leq u(y_k,y_q, \dots)]
    % \times
    u(y_k,y_q, \dots)
    % \times
    W_{q}^{k} 
    % \times 
    \left(
    \sum_{q=1}^{n}{
        [
            u(y_k,y_k, \dots)
            \leq
            u(y_k,y_q, \dots)
        ]
        w_q
    }
\right) ^ {-1}
\right),
```

where 

```math
\text{Pr}[q | k] = 
W_{q}^{k} \times \left(
    \sum_{q=1}^{n}{
        [
            u(y_k,y_k, \dots)
            \leq
            u(y_k,y_q, \dots)
        ]
        w_q
    }
\right) ^ {-1}
```
is the probability of being able and willing to find a job in occupation $q$, given person $k$'s skill set, preferences and overall career strategy.

- p.s.: note the importance of employment in determining the probability of a career path:
```math
\argmax_{q}\left(
    [
        u(y_k,y_k, \dots)
            \leq
            u(y_k,y_q, \dots)
    ]
    u(y_k,y_q, \dots)
    W_{q}^{k}
\right),
```