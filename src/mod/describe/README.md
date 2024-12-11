# `describe` module
This module contains coefficients that are calculated using a single skill set. They don't require direct comparison with other skill sets, but are *descriptive* and can, thus, be thought of as "properties" of a skill set. 

## `eqvl` (equivalence) submodule
- `aeq`: attribute equivalence is calculated for each attribute in a skill set and indicates its importance. 

## `gene` (generality) submodule
A skill set's generality determines whether one is a generalist or specialist.
- Methods:
    - Weighted Penalized Mode (`mean`):
        - Mathematically equivalent to the mean of a maxima-normalized skill set.

## `comp` (competence) submodule
A skill set's competence describes the magnitude of one's overall skill level.
- Methods:
    - Weighted Penalized Mode (`mean`):
        - Mathematically equivalent to the attribute-equivalence-weighted mean of a skill set
        - Implicitly assumes attributes are substitutes
            - This makes sense when speaking of overall competence
            - While assuming complementarity makes sense when speaking of relative competence
                - i.e. productivity in a given occupation, or matching percentages
    - Cobb-Douglas (`cobb-douglas`):
        - Frames the concept of competence in terms of aggregate production
        - Implicitly assumes attributes are complementary
    - Geometric Mean (removed):
        - Attribute-equivalence-weighted geometric mean
        - Implicitly assumes attributes are complementary
        - Mathematically equivalent to Cobb-Douglas
- Note:
    - Attribute complementarity vs competence
        - When we speak of productivity and employability, we are correct to assume attributes are complementary, for occupations are self-contained, coherent, collections of tasks, all of which depend on their entire skill set
        - Furthermore, to be productive is to be competent in a particular domain, with established requirements
        - However, to be competent overall is not the same thing as being competent in a particular domain, because one can have skills that do not complement one another
            - That is, while an occupation has established requirements and, thus, a determined metric of productivity, a skill set in and of itself can be suitable for many such occupations and, therefore, its attributes don't have to complement one another in a holistic manner, as occupational operations (see paper on employability)
    - Employable != Competent: one is said to be competent if they are good at what they do. Being good at many things does not make one more competent, all else being equal, although it makes one more employable. For instance, a pilot is no more competent if they also know how to cut hair; however, if they do also know how to cut hair, that makes them more employable (as a barber). And, conversely, if a barber can fly an airplane, that does not make them more competent overall, only more employable.
    - Matching, in this sense, is a relative measure of competence (i.e. to be similar with an occupation, and productive in it, means one is competent at its tasks): therefore, to be competent is to be good at what one does, while being employable is to be good at many things. Overall competence, then, is not affected by the quantity of skills, but rather by their quality.