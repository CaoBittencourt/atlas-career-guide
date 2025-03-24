# setup
# region: imports
box::use(
  gn = mod / describe / gene,
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  stats[weighted.mean]
)

# endregion
# methods
# region: general method
comp.general <- function(ak) {
  # assert args in main function
  # expected value of all competencies
  return(mean(ak))
}

# endregion
# region: expertise method
comp.expertise <- function(ak, äk) {
  # assert args in main function
  # expected value of actual competencies
  return(weighted.mean(ak, äk))
}

# endregion
# region: list of methods
list(
  "general" = "general",
  "expertise" = "expertise"
) -> comp.methods

# endregion
# dispatch
# region: comp generic function
comp <- function(skill_set, comp_method = comp.methods$expertise, ...) {
  # assert args
  assert$models$validate.skill.set(skill_set)
  assert$base$validate.method(comp_method, "comp_method", comp.methods)

  # estimate attribute equivalence
  if (comp_method[[1]] == comp.methods$expertise) {
    eq$aeq |> do.call(args = list(skill_set) |> c(list(...))) -> äk
  }

  # multiple dispatch
  if (comp_method[[1]] == comp.methods$expertise) {
    return(comp.expertise(skill_set, äk))
  }

  if (comp_method[[1]] == comp.methods$general) {
    return(comp.general(skill_set))
  }
}

# endregion
# exports
# region: exports
box::export(comp, comp.methods)

# endregion
