# region: imports
box::use(
  assert = mod / utils / assert,
  gn = mod / describe / gene,
  glogis = mod / utils / logistic
)

# endregion
# region: method 1: linear
aeq.linear <- function(skill_set) {
  # assert args in main function
  # linear attribute equivalence
  return(skill_set / max(skill_set))
}

# endregion
# region: method 2: specialty-root
aeq.specialty_root <- function(skill_set, generality) {
  # assert args in main function
  # take the root of skill set with respect to specialty (i.e. 1 - generality)
  return(skill_set^(1 / (1 - generality)))
}

# endregion
# region: method 3: linear-logistic
aeq.linear_logistic <- function(skill_set, generality) {
  # assert args in main function
  # apply generalized logistic function with parameters
  return(
    glogis$logistic(
      x = skill_set,
      m = generality,
      a = 0,
      k = skill_set,
      c = 1,
      q = generality * (1 - skill_set),
      nu = skill_set / generality,
      b = 1 / (1 - generality)
    )
  )
}

# endregion
# region: attribute equivalence generic
aeq <- function(skill_set, generality = NULL, method = c("linear-logistic", "specialty-root", "linear")[[1]]) {
  # assert args
  assert$valid_skill_set(skill_set)

  stopifnot(
    "'method' must be one of the following methods: 'linear-logistic', 'specialty-root', 'linear'." = any(
      method == c("linear-logistic", "specialty-root", "linear")
    )
  )

  stopifnot(
    "'generality' must be either numeric or NULL." = any(
      generality[[1]] |> is.numeric(),
      generality[[1]] |> is.null()
    )
  )

  # estimate skill set generality
  if (is.null(generality[[1]])) {
    gn$gene(skill_set) -> generality
  }

  # multiple dispatch
  switch(method[[1]],
    "linear-logistic" = return(aeq.linear_logistic(skill_set, generality[[1]])),
    "specialty-root" = return(aeq.specialty_root(skill_set, generality[[1]])),
    "linear" = return(aeq.linear(skill_set))
  )
}

# endregion
# region: exports
box::export(aeq)

# endregion
