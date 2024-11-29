# # region: box modules
# # install box if not installed
# if (!any(utils::installed.packages()[, 1] == "box")) {
#   install.packages("box", dependencies = T)
# }

# # set working directory
# setwd(box::file())

# # set modules path
# options(box.path = file.path(getwd(), "src"))

# # endregion
# # region: imports

# # endregion
# region: assert skill set
valid_skill_set <- function(x) {
  stopifnot(
    '"skill_set" must be a numeric vector."' =
      is.numeric(x)
  )
}

# endregion
# region: exports
box::export(
  valid_skill_set
)

# endregion
