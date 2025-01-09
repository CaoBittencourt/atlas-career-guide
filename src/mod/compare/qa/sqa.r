# region: imports
box::use(
  mod / compare / qa / uqa[...]
)

# endregion
# region: sufficient qualification
sqa <- function(skill_set, skill_mtx, aeq_method = NULL) {
  # assert args
  # estimate (un)weighted sufficient qualification coefficient
  return(1 - uqa(skill_set, skill_mtx, aeq_method))
}

# endregion
# region: exports
box::export(sqa)

# endregion
