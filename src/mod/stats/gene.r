# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# set modules path
options(box.path = file.path(getwd(), "src"))

# endregion
# region: imports
box::use(util = mod / utils)

# endregion
# region: skill set generality
gene <- function(skill_set) {
  # assert args
  util$assert$valid_skill_set(skill_set)

  # return skill set generality
  return(mean(skill_set / max(skill_set)))
}

# endregion
# region: exports
box::export(gene)

# endregion
