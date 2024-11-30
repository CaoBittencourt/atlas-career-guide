# region: imports
box::use(
  assert = mod / utils / assert,
  gn = mod / stats / gene
)

# endregion
# region: attribute equivalence
aeq <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # remove generality from attribute equivalence?
  return(skill_set / max(skill_set))
}

# endregion
# region: workforce equivalence

# endregion
# region: equivalent similarity

# endregion
# region: equivalence generic

# endregion
# region: exports
box::export(aeq)

# endregion
