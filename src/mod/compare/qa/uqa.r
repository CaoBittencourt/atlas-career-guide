# region: imports
box::use(
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  mod / utils / gap[...],
  dplyr[bind_rows]
)

# endregion
# region: underqualification
uqa <- function(skill_set, skill_mtx, aeq_method = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)
  assert$as.skill_mtx(skill_mtx) -> skill_mtx

  # default to unweighted
  weights_mtx <- 1

  # if attribute equivalence method is provided,
  # estimate attribute equivalence matrix
  if (length(aeq_method)) {
    skill_mtx |>
      lapply(eq$aeq, aeq_method = aeq_method) |>
      bind_rows() ->
    weights_mtx
  }

  # estimate (un)weighted underqualification coefficient
  return(colSums(weights_mtx * gap(skill_mtx, skill_set)) / colSums(weights_mtx * skill_mtx))
}

# endregion
# region: exports
box::export(uqa)

# endregion
