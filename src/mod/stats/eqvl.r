# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

project.root()

# # set modules path
# options(box.path = file.path(getwd(), "src"))

# endregion
# region: imports
# box::use(util = mod / module_exports_file)

# endregion
# region: attribute equivalence
# aeq <- function(skill_set, generality = NULL){

#   # remove generality from attribute equivalence?
#   # assert args


# }

# endregion
# region: workforce equivalence

# endregion
# region: equivalent similarity

# endregion
# region: exports
box::export(
  # functions to export
  # if blank, exports all functions
)

# endregion
