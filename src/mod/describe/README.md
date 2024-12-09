# `describe` submodule
This module contains coefficients that can be calculated using a single skill set. They do not require comparison with any other metrics, but are, rather, *descriptive* and can, therefore, be thought of as "properties" of a skill set. 

They are:
- Generality: a skill set's generality determines whether one is a generalist or specialist.
    - Methods:
        - Weighted Penalized Mode: equivalent to the mean of a maxima-normalized skill set.
- Competence: a skill set's competence describes the magnitude of one's overall skill level.
    - Methods:
        - Weighted Penalized Mode: equivalent to the attribute-equivalence-weighted mean of a skill set.
        - Geometric Mean: emphasizes attribute complementarity, usually yields higher coefficients.
        - Cobb-Douglas: frames the concept of competence in terms of aggregate production, usually yields higher coefficients than weighted penalized mode, but still much lower than geometric mean.
    - Note:
        - Employable != Competent: one is said to be competent if they are good at what they do. Being good at many things does not make one more competent, all else being equal, although it makes one more employable. Therefore, a pilot is no more competent if they also know how to cut hair; however, if they do also know how to cut hair, that makes them more employable (as a barber). Matching, in this sense, is a relative measure of competence (i.e. to be similar with an occupation, and productive in it, means of is competent at its attributes): therefore, to be competent is to be good at what one does, while being employable is to be good at many things. Overall competence, though, is not affected by the quantity of skills, only by their quality.
- Equivalence: attribute equivalence is calculated for each attribute within a skill set and indicates its importance. 