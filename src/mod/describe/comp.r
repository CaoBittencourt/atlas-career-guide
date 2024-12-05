# region: imports
box::use(
  gn = mod / describe / gene,
  eq = mod / describe / eqvl,
  assert = mod / utils / assert,
  stats[weighted.mean]
)

# endregion
# region: skill set competence
comp <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> eq$aeq()
    )
  )
}

# endregion
# region: exports
box::export(comp)

# endregion
