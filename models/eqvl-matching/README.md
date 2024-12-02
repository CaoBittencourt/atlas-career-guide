# eqvl-matching
## motivation
- assure attribute equivalence as a linear-logistic curve of maxima-normalized attributes with generality as the midpoint is a better weight metric than linear weights

## notes
### on generality vs attribute equivalence
- using generality as the midpoint for linear-logistic-maxima-normalized attribute equivalence makes sense because generality is the mean of maxima-normalized attribute scores:
    - ã_k := a_k / max(a_k)
    - gamma_k := mean(ã_k)
    - ä_k := æq(ã_k, gamma_k(ã_k))
- therefore, generality determines the mean attribute score relative to a skill set's highest attribute
- it is, thus, arguably an objective midpoint for differentiating competencies:
    - if item scores are above the generality midpoint, they are closer to a skill set's highest attribute than the mean of all its attributes
    - and vice-versa if item scores are below the maxima-normalized mean (i.e. the generality midpoint)
    - the more item scores are above the generality midpoint, the more they can be considered "core attributes"
    - the more item scores are below the generality midpoint, the more they can be considered "irrelevant"
- in addition, it is intuitive to assume specialists rely more on their "core attributes" (i.e. their specialty) than generalists
- furthermore, it is unjust to judge an occupation's competence based on their performance on attributes irrelevant to it. For, as we have said before:
    - "Indeed, no one would say, for instance, that an airline pilot is less of a pilot if they also know how to cut hair, just as we do not say a barber is less of a barber if they cannot fly an airplane. Put another way, competencies that have 'nothing to do' with an occupation should not be a limiting factor to career compatibility." (Bittencourt, 2024, "Introduction to Quantitative Career Matching")
        - Of course, this is not only applicable in the context of career matching. So, here also, we might say such competencies should not be a limiting factor when assessing the overall competence of a skill set.

### on similarity
- it doesn't make sense to apply overqualification substitution when speaking of similarity per se
- similarity determines which occupations (skill set vectors) are closer to a given career profile
- this is correlated, but is not equivalent, to productivity and employability
- indeed, one can be highly productive and employable in an occupation without being very similar
    - i.e. if one is overqualified
- and one can also be relatively similar without being highly employable
    - i.e. if most of an occupation's tasks concentrate on the higher end of the difficulty scale
- productivity and employability also depend on a time allocation function, but similarity does not
- usually, when choosing a career, we should use the similarity, instead of the productivity metric
- however, if one is looking for a job right now, employability is the correct metric to use

## conclusion
- best models:
    - Euclidean with linear-logistic-maxima-normalized attribute equivalence as weights
    - probit with linear-logistic-maxima-normalized attribute equivalence as weights
    - logit with linear-logistic-maxima-normalized attribute equivalence as weights