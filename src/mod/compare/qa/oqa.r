# setup
# region: imports
box::use(
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  mod / utils / gap[...],
  mod / utils / cbindmap[...],
  dplyr[bind_rows],
  mod / utils / conform[...]
)

# endregion
# dispatch
# region: overqualification
oqa <- function(skill_set, skill_mtx, ...) {
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

  # estimate (un)weighted overqualification coefficient
  # return(colSums(weights_mtx * gap(skill_set, skill_mtx)) / colSums(weights_mtx * gap(1, skill_mtx)))
  colSums(weights_mtx * gap(1, skill_mtx)) -> max.qa

  return(
    skill_set |>
      conform(skill_mtx) |>
      cbindmap(
        function(Ak) {
          colSums(weights_mtx * gap(Ak, skill_mtx)) / max.qa
        },
        to = names(skill_mtx)
      )
  )
  # return(
  #   skill_set |>
  #     conform(skill_mtx) |>
  #     lapply(
  #       function(Ak) {
  #         colSums(weights_mtx * gap(Ak, skill_mtx)) / max.qa
  #       }
  #     )
  # )
}

# endregion
# exports
# region: exports
box::export(oqa)

# endregion
