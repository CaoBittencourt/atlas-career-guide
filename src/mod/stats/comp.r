# region: modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

# objective project root
modular::project.root(root.name = "src.root")

# box module search path
options(box.path = getwd())

# endregion
# region: imports
box::use(
  gn = mod / stats / gene,
  eq = mod / stats / eqvl,
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

comp2 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> eq$aeq2()
    )
  )
}

# rm(skills)

# endregion
# region: exports
box::export(comp)

# endregion
