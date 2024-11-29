# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# set modules path
options(box.path = file.path(getwd(), "src"))

# endregion
# region: imports
# box::use(util = mod / module_exports_file)

# endregion
# region: function name

# endregion
# region: function name

# endregion
# region: exports
box::export(
  # functions to export
  # if blank, exports all functions
)

# endregion
