# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# modules search path
options("box.path" = file.path(box::file(), "modules"))

# module exports
#' @export
box::use(
  # cp = modules / stats / comp,
  gn = modules / stats / gene
  # eq = modules / stats / eqvl
)
