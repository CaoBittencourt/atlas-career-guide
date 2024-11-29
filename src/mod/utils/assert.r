# # region: modules
# # install box if not installed
# if (!any(utils::installed.packages()[, 1] == "box")) {
#   install.packages("box", dependencies = T)
# }

# # install modular if not installed
# if (!any(utils::installed.packages()[, 1] == "modular")) {
#   devtools::install_github("CaoBittencourt/modular")
# }

# # objective project root
# modular::project.root(root.name = "src.root")

# # box module search path
# options(box.path = getwd())

# # endregion
# # region: imports
# # box::use(util = mod / module_exports_file)

# # endregion
# region: assert skill set
valid_skill_set <- function(x) {
  stopifnot(
    '"skill_set" must be a numeric vector in the unit interval."' = all(
      is.numeric(x), x <= 1, x >= 0
    )
  )
}

# endregion
# region: exports
box::export(
  valid_skill_set
)

# endregion
