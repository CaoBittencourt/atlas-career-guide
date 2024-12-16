# `compare` module
As opposed to descriptive models, comparative models cannot be estimated with a single quantity or vector.

## pairwise
Binary models are calculated by comparing two skill sets or quantities
- Skill Set Similarity (`match` submodule)
- Interchangeability or sufficient similarity (`match` submodule)
- Field Similarity (`field` submodule)
- Qualification (`qa` submodule)
- Productivity (`prod` submodule)
- Education and Experience Equivalence (`eeq` submodule)
- Hireability (`hire` submodule)
- Utility Optimization (`joy` submodule)
- Employability (moved to `labor` module)

## aggregate
Models that aggregate multiple pairwise models into a single value
- Aggregate employability (moved to `labor` module)
- Labor market competitiveness (moved to `labor` module)

## macroeconomic
These models mobilize previous concepts into higher-level abstractions
- Economic Taxonomy (moved to `macro` module)
- Dynamic Labor Markets (moved to `labor` module)