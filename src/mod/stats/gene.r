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
