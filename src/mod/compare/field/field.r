# setup
# region: imports
box::use(
  mod / utils / assert,
  mod / utils / math[cosine.similarity],
  s = mod / compare / similarity,
  # mod / compare / similarity / methods / cobb_douglas,
  mod / utils / data[vmap, conform],
)

# endregion
# methods
# region: cosine method
field.cosine <- function(aeq_set, aeq_mtx) {
  # assert args in main function
  # vmap cosine similarity
  return(aeq_set |> vmap(aeq_mtx, cosine.similarity))
}

# endregion
# region: cobb_douglas method
field.cobb_douglas <- function(aeq_set, aeq_mtx) {
  # assert args in main function
  # apply cobb-douglas similarity
  return(s$similarity(aeq_set, aeq_mtx, s$similarity.methods$cobb_douglas, bind = F)[[1]])
}

# endregion
# region: list of methods
list(
  "cosine" = "cosine",
  "cobb_douglas" = "cobb-douglas"
) -> field.methods

# endregion
# dispatch
# region: field generic function
field <- function(aeq_set, aeq_mtx, field_method = field.methods[[1]], ...) {
  # assert args
  assert$models$as.aeq.matrix(aeq_set, "aeq_set") -> aeq_set
  assert$models$as.aeq.matrix(aeq_mtx, "aeq_mtx") -> aeq_mtx

  # multiple dispatch
  if (field_method[[1]] == field.methods$cosine) {
    return(field.cosine(aeq_set, aeq_mtx))
  }

  if (field_method[[1]] == field.methods$cobb_douglas) {
    return(field.cobb_douglas(aeq_set, aeq_mtx))
  }
}

# endregion
# exports
# region: exports
box::export(field, field.methods)

# endregion
