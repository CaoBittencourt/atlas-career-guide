# region: imports
box::use(
  mod / compare / qa / uqa[...]
)

# endregion
# region: sufficient qualification
sqa <- function(skill_set, skill_mtx, aeq_method = NULL) {
  # assert args
  # estimate (un)weighted sufficient qualification coefficient
  return(
    uqa(skill_set, skill_mtx, aeq_method) |>
      lapply(
        function(k) {
          1 - k
        }
      )
  )
}

# endregion
# region: exports
box::export(sqa)

# endregion
