# models: statistical models sketches
## eqvl-comp
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