# region: modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# # install modular if not installed
# if (!any(utils::installed.packages()[, 1] == "modular")) {
#   devtools::install_github("CaoBittencourt/modular")
# }

# # objective project root
# modular::project.root(root.name = ".atlas")

# # box module search path
# options(box.path = getwd())

# endregion
# region: imports
box::use(
  assert = utils / assert,
  gn = stats / gene
  # gn = mod / stats / gene
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
