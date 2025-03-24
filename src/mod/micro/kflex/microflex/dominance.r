# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  dom = mod / compare / dom,
  mod / micro / kflex / microflex / microflex[...],
)

# endregion
# dispatch
# region: attribute dominance generic function
dominance.skill <- function(skill_mtx, employment = NULL, skill.names = NULL, aggregate = T) {
  # estimate capital microflexibility matrix, then pass it to the dominance function
  return(skill_mtx |> microflex(employment, skill.names) |> dom$dominance(NULL, aggregate))
}

# endregion
# exports
# region: exports
box::export(dominance.skill)

# endregion
