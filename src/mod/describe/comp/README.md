# `comp`: assessing skill set competence

To assess an individual's *general competence* we take the mean of their skill set,

```math
\begin{gather}
c_k := 
\left(
    \frac{1}{m}
\right)
\sum_{i=1}^{m}
a_{i}^{k}
\in
[0,1]
.
\end{gather}
```

In other words, we estimate *competence* by aggregating all *competencies* into a single value which best represents them. This  is the expected value of one's attributes and so, it is a reliable estimate of their overall performance in any given task, hence their competence.

However, real labor markets are often stratified (i.e. not everyone does everything) and efficient economies very much depend on Smithian growth and similar phenomena for their prosperity. Furthermore, we have already quantified skill set's generality and noted some occupations are, indeed, more specialized than others. Therefore, it doesn't seem "fair" to judge their competence based solely on coefficient (1); we should account for each attributes' importance for each skill set in order to properly understand them: 

```math
\begin{gather}
\ddot{c}_k := 
\frac{
    \sum_{i=1}^{m}
    \ddot{a}_{i}^{k}
    a_{i}^{k}
}{
    \sum_{i=1}^{m}
    \ddot{a}_{i}^{k}
}
\in
[0,1]
.
\end{gather}
```

The equation above uses the attribute equivalence coefficient to weigh each competency, so that aggregate competence corresponds to the expected value of one's *actual* attributes. For this reason, we term (2) "equivalent competence" or *expertise* (the *umlaut* being employed, here also, as a mathematical operator for the concept of "equivalence" and, in this case, "expertise" as well, both of which start with the letter "e", in keeping with the german diacritic's origin).

Thus, we have two measures of competence: a *general competence* estimation of the expected value of one's skill set (1); and (2), an *effective competece* (or equivalent) estimation of one's expertise (again, the expected value of one's actual attributes).