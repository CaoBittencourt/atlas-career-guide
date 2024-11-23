# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# modules search path
options("box.path" = file.path(box::file(), "modules"))

# endregion
# region: imports
box::use(utils = modules/utils)
utils$assert$assert_skill_set()

# endregion
# region: skill set generality
gene <- function(skill_set) {
  # assert args
  assert$is.skill_set(skill_set)

  # return skill set generality
  return(mean(skill_set / max(skill_set)))
}

# endregion
