# `eg`: `egmap` implementations for research and development

Ceteris paribus, this module should not be used in production, because vectorized functions are faster than `egmap`, whose purpose is mainly for comparing the various methods of a given function.

```r
# import eg
box::use(eg)

# load skill set matrix from local dir
getOption("atlas.skills_mtx") |> readRDS() ->  occupations

# expand grid map similarity function
occupations[-1] |> 
    eg$similarity(
        occupations[-1], 
        match_method = c(
            "euclidean", 
            "bvls"
        )
    )
```