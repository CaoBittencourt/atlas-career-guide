# region: imports
box::use(
  gn = mod / describe / gene,
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  stats[weighted.mean]
)

# endregion
# region: general competence coefficient
comp.general <- function(ak) {
  # assert args in main function
  # expected value of all competencies
  return(mean(ak))
}

# endregion
# region: equivalent competence coefficient (expertise)
comp.expertise <- function(ak, äk) {
  # assert args in main function
  # expected value of actual competencies
  return(weighted.mean(ak, äk))
}

# endregion
# region: list of methods
list(
  "expertise" = "expertise",
  "general" = "general"
) -> comp.methods

# endregion
# region: generic function
comp <- function(skill_set, comp_method = comp.methods[[1]], ...) {
  # assert args
  assert$valid_skill_set(skill_set)
  assert$valid_method(comp_method, comp.methods, "comp_method")

  # estimate attribute equivalence
  if (comp_method[[1]] == comp.methods$expertise) {
    eq$aeq |> do.call(args = list(skill_set) |> c(list(...))) -> äk
  }

  # multiple dispatch
  comp_method[[1]] |>
    switch(
      "general" = return(comp.general(skill_set)),
      "expertise" = return(comp.expertise(skill_set, äk))
    )
}

# endregion
# region: exports
box::export(comp, comp.methods)

# endregion
