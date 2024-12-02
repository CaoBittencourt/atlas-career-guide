# eqvl-comp
## motivation
- skill set generality is already settled
- assess which attribute equivalence and competence metrics make more sense
- old metrics:
    - ã_k := a_k / max(a_k)
    - gamma_k := mean(ã_k)
    - ä_k := æq(ã_k, gamma_k(ã_k)) := linear-logistic with generality as the function's midpoint
    - c_k := weighted.mean(x = a_k, w = ä_k)
- new metrics:
    - ã_k := a_k / max(a_k)
    - ä_k := æq(a_k) := ã_k := a_k / max(a_k)
    - gamma_k := mean(ã_k) := mean(ä_k)
    - c_k := weighted.mean(x = a_k, w = ä_k)

## conclusion
<!-- - competence does changes significantly when using either attribute equivalence metric
- competence is always higher with attribute equivalence metric 2, for it emphasizes core competencies
- however, attribute equivalece metric 1 is just the same as using linear weights, which we have found in the past overestimates similarity scores
- in addition, with attribute equivalence metric 1, the very concept of attribute equivalence is much less relevant
- we should evaluate whether tha same holds true for matching results -->