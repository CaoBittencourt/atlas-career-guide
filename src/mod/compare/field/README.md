# `field`
Field similarity determines if two skill sets can be said to be in the same "field" within the industry (e.g. Finance, Engineering, Fine Arts, etc). By definition, then, this concept is not concerned with vector magnitude, only its direction: we do not care about assessing competence here, but rather classifying occupations with respect to their field of activity (i.e. whether they deal with the same "sorts of things").

This implies the appropriate mathematical matching method for this is cosine similarity:

$$

\begin{gather}
s_{kq}^{\theta} := 
\cos\theta_{kq} := 
\frac{
    \boldsymbol{\ddot{a}_{k} \ \cdot \boldsymbol{\ddot{a}_{q}}}
}{
    \left\lVert{\boldsymbol{\ddot{a}_{k}}}\right\lVert
    \left\lVert{\boldsymbol{\ddot{a}_{q}}}\right\lVert
}
:=
\frac{
    \sum_{i=1}^{m} \ddot{a}_{i}^{k} \ddot{a}_{i}^{q}
}{
    \sqrt{
        \sum_{i=1}^{m} \ddot{a}_{i}^{k} \ddot{a}_{i}^{k}
    }
    \sqrt{
        \sum_{i=1}^{m} \ddot{a}_{i}^{k} \ddot{a}_{i}^{k}
    }
}
\in
[0,1]
.
\end{gather}

$$

In addition, the matrix of all field similarities in the economy,
$
\textbf{S}_{\boldsymbol{\Theta}} \in [0,1] ^ {m \times m}
,
$
can be employed in clustering algorithms where the level of competence is not important (e.g. to determine data-driven, instead of semantic, occupational taxonomies, see `taxa` module).
