# region: imports
box::use(
  gn = mod / describe / gene,
  eq = mod / describe / eqvl,
  assert = mod / utils / assert,
  stats[weighted.mean]
)

# endregion
# region: weighted penalized mlv (i.e. mean) method
comp.mean <- function(skill_set, generality = NULL) {
  # assert args in main function
  # skill set competence as aeq-weighted mean
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> eq$aeq(generality)
    )
  )
}

# endregion
# region: cobb-douglas method
comp.cobb_douglas <- function(skill_set, generality = NULL) {
  # assert args in main function
  # skill set competence as cobb-douglas production function
  eq$aeq(generality) -> ä
  return(prod(skill_set^(ä / sum(ä))))
}

# endregion
# region: skill set competence generic
comp <- function(skill_set, generality = NULL, method = c("mean", "cobb-douglas")[[1]]) {
  # assert args
  assert$valid_skill_set(skill_set)
  assert$valid_generality(generality)

  stopifnot(
    "'method' must be one of the following methods: 'mean', 'cobb-douglas'." = any(
      method == c("mean", "cobb-douglas")
    )
  )

  # estimate skill set generality
  if (is.null(generality[[1]])) {
    gn$gene(skill_set) -> generality
  }

  # multiple dispatch
  switch(method[[1]],
    "mean" = return(comp.mean(skill_set, generality = NULL)),
    "cobb-douglas" = return(comp.cobb_douglas(skill_set, generality = NULL))
  )
}

# endregion
# region: exports
box::export(comp)

# endregion
