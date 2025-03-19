# region: imports
box::use(
  assert = mod / utils / assert,
  gn = mod / describe / gene,
  glogis = mod / utils / logistic
)

# endregion
# region: linear_logistic method
aeq.linear_logistic <- function(ã, gammak) {
  # assert args in main function

  # midpoint
  m <- 1 - gammak

  # apply generalized logistic function with parameters
  return(
    glogis$logistic(
      x = ã,
      m = m,
      a = 0,
      k = ã,
      c = 1,
      q = m * (1 - ã),
      nu = ã / (m * (ã != 1)),
      # nu = ã / m,
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
  'linear_logistic' = 'linear_logistic',
  'gene_root' = 'gene_root',
  'linear' = 'linear'
) -> aeq.methods

# endregion
# region: aeq generic function
aeq <- function(skill_set, generality = NULL, aeq_method = aeq.methods[[1]], ...) {
  # assert args
  assert$valid_skill_set(skill_set)
  assert$valid_method(aeq_method, aeq.methods, 'aeq_method')

  # multiple dispatch
  aeq_method[[1]] |>
    switch(
      aeq.methods$linear_logistic = return(aeq.linear_logistic()),
      aeq.methods$gene_root = return(aeq.gene_root()),
      aeq.methods$linear = return(aeq.linear())
    )
}

# endregion
# region: exports
box::export(aeq, aeq.methods)

# endregion
