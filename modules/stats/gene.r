# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# # modules search path
# options("box.path" = file.path(box::file(), "modules"))

# endregion
# region: imports
# box::use(assert = modules / utils / assert)

# endregion
# region: skill set generality
#' @export
gene <- function(skill_set) {
  # # assert args
  # assert$valid_skill_set(skill_set)

  # return skill set generality
  return(mean(skill_set / max(skill_set)))
}

# endregion
