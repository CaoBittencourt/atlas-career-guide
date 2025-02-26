# region: imports
box::use(
  mod / compare / qa / uqa[...],
)

# endregion
# region: sufficient qualification
sqa <- function(skill_set, skill_mtx, aeq_method = NULL) {
  # assert args
  # estimate (un)weighted sufficient qualification coefficient
  uqa_mtx <- uqa(skill_set, skill_mtx, aeq_method)
  1 - uqa_mtx[
    uqa_mtx |> vapply(is.numeric, logical(1))
  ] ->
  uqa_mtx[
    uqa_mtx |> vapply(is.numeric, logical(1))
  ]

  return(uqa_mtx)
}

# endregion
# region: exports
box::export(sqa)

# endregion
