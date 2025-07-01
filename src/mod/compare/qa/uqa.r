# setup
# region: imports
box::use(
  eq = describe / aeq,
  assert = utils / assert,
  utils / gap[...],
  utils / cbindmap[...],
  dplyr[bind_rows],
  utils / conform[...]
)

# endregion
# dispatch
# region: underqualification
uqa <- function(skill_set, skill_mtx, ...) {
  # assert args
  # assert$valid_skill_set(skill_set)
  assert$models$as.skill.set.matrix(skill_set, "skill_set") -> skill_set
  assert$models$as.skill.set.matrix(skill_mtx, "skill_mtx") -> skill_mtx

  # default to unweighted
  weights_mtx <- 1

  # if attribute equivalence method is provided,
  # estimate attribute equivalence matrix
  list(...) -> dots

  if (length(dots$aeq_method)) {
    skill_mtx |>
      lapply(eq$aeq, aeq_method = dots$aeq_method) |>
      bind_rows() ->
    weights_mtx
  }

  # estimate (un)weighted underqualification coefficient
  colSums(weights_mtx * skill_mtx) -> max.qa

  return(
    skill_set |>
      conform(skill_mtx) |>
      cbindmap(
        function(Ak) {
          colSums(weights_mtx * gap(skill_mtx, Ak)) / max.qa
        },
        to = names(skill_mtx)
      )
  )
  # return(
  #   skill_set |>
  #     conform(skill_mtx) |>
  #     lapply(
  #       function(Ak) {
  #         colSums(weights_mtx * gap(skill_mtx, Ak)) / max.qa
  #       }
  #     )
  # )
  # return(colSums(weights_mtx * gap(skill_mtx, skill_set)) / colSums(weights_mtx * skill_mtx))
}

# endregion
# exports
# region: exports
box::export(uqa)

# endregion
