# region: imports
box::use(
  assert = mod / utils / assert,
  gn = mod / describe / gene,
  glogis = mod / utils / logistic
)

# endregion
# region: linear method
aeq.linear <- function(ã) {
  # assert args in main function
  # linear attribute equivalence
  return(ã)
}

# endregion
# region: specialty-root method
aeq.specialty_root <- function(ã, generality) {
  # assert args in main function
  # take the root of skill set with respect to specialty (i.e. 1 - generality)
  return(ã^(1 / (1 - generality)))
}

# endregion
# region: linear-logistic method
aeq.linear_logistic <- function(ã, generality) {
  # assert args in main function
  
  # variable
  x <- ã

  # midpoint
  m <- 1 - generality
  
  # apply generalized logistic function with parameters
  return(
    glogis$logistic(
      x = x,
      m = m,
      a = 0,
      k = x,
      c = 1,
      q = m * (1 - x),
      nu = x / m,
      b = 1 / (1 - m)
    )
  )
}

# endregion
# region: attribute equivalence generic
aeq <- function(skill_set, generality = NULL, aeq_method = c("linear-logistic", "specialty-root", "linear")[[1]]) {
  # assert args
  assert$valid_skill_set(skill_set)
  assert$valid_generality(generality)

  stopifnot(
    "'aeq_method' must be one of the following methods: 'linear-logistic', 'specialty-root', 'linear'." = any(
      aeq_method == c("linear-logistic", "specialty-root", "linear")
    )
  )

  # estimate skill set generality
  if (all(
    aeq_method != "linear",
    is.null(generality[[1]])
  )) {
    gn$gene(skill_set) -> generality
  }

  # maxima-normalized attributes
  ã <- skill_set / max(skill_set)

  # multiple dispatch
  aeq_method[[1]] |>
    as.character() |>
    switch(
      "linear-logistic" = return(aeq.linear_logistic(ã, generality[[1]])),
      "specialty-root" = return(aeq.specialty_root(ã, generality[[1]])),
      "linear" = return(aeq.linear(ã))
    )
}

# endregion
# region: exports
box::export(aeq)

# endregion
