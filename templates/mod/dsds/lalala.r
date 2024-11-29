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
modular::project.root(root.name = "atlas.root")

# box module search path
options(box.path = getwd())

# endregion
# region: imports
# box::use(dsds = src / mod / dsds)

# endregion
# region: functions

# endregion
# region: functions

# endregion
# region: exports
box::export(
  # functions to export
  # if blank, exports all functions
)

# endregion
