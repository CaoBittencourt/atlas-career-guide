# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  s = mod / compare / similarity
)

# endregion
# methods
# region: list of methods
s$similarity.methods[
  c("cobb_douglas", "gmme")
] -> productivity.methods

# endregion
# dispatch
# region: productivity generic function
productivity <- function(skill_set, skill_mtx, productivity_method = productivity.methods[[1]], ...) {
  # assert args in similarity function
  # multiple dispatch in similarity function
  return(
    skill_set |>
      s$similarity(
        skill_mtx,
        productivity_method,
        ...
      )
  )
}

# endregion
# exports
# region: exports
box::export(productivity, productivity.methods)

# endregion
