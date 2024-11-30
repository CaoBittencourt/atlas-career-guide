# region: imports
box::use(assert = mod / utils / assert)

# endregion
# region: skill set generality
gene <- function(skill_set) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set generality
  return(mean(skill_set / max(skill_set)))
}

# endregion
# region: exports
box::export(gene)

# endregion
