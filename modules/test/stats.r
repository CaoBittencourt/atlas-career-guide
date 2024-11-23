# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# modules search path
options("box.path" = file.path(box::file(), "modules"))

# imports
box::use(dsds = modules / utils)
box::use(modules / dsdsds)

# test generality coefficient
