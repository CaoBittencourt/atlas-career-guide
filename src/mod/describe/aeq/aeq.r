# setup
# region: imports
box::use(
  assert = utils / assert,
  gn = describe / gene,
  glogis = utils / logistic,
  stats[setNames]
)

# endregion
# methods
# region: linear_logistic method
aeq.linear_logistic <- function(ãk, gammak) {
  # assert args in main function

  # midpoint
  m <- 1 - gammak

  # apply generalized logistic function with parameters
  return(
    glogis$logistic(
      x = ãk,
      m = m,
      a = 0,
      k = ãk,
      c = 1,
      q = m * (1 - ãk),
      nu = ãk / (m * (ãk != 1)),
      # nu = ãk / m,
      b = 1 / (1 - m)
    )
  )
}

# endregion
# region: gene_root method
aeq.gene_root <- function(ãk, gammak) {
  # assert args in main function
  # take the root of skill set with respect to generality
  return(ãk^(1 / gammak))
}

# endregion
# region: linear method
aeq.linear <- function(ãk) {
  # assert args in main function
  # linear attribute equivalence
  return(ãk)
}

# endregion
# region: list of methods
list(
  "linear_logistic" = "linear-logistic",
  "gene_root" = "gene-root",
  "linear" = "linear"
) -> aeq.methods

# endregion
# dispatch
# region: aeq generic function
aeq <- function(skill_set, generality = NULL, aeq_method = aeq.methods[[1]], ...) {
  # assert args
  assert$models$validate.skill.set(skill_set, "skill_set", nullable = F)
  assert$models$validate.gene(generality, "generality", nullable = T)
  assert$base$validate.method(aeq_method, "aeq_method", aeq.methods)

  # estimate skill set generality
  if (all(is.null(generality), aeq_method[[1]] != aeq.methods$linear)) {
    gn$gene(skill_set) -> generality
  }

  # multiple dispatch
  if (aeq_method[[1]] == aeq.methods$linear_logistic) {
    return(aeq.linear_logistic(skill_set, generality))
  }

  if (aeq_method[[1]] == aeq.methods$gene_root) {
    return(aeq.gene_root(skill_set, generality))
  }

  if (aeq_method[[1]] == aeq.methods$linear) {
    return(aeq.linear(skill_set))
  }
}

# endregion
# exports
# region: exports
box::export(aeq, aeq.methods)

# endregion
