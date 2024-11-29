# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# set modules path
options(box.path = file.path(getwd(), "src"))

#' @export
box::use(
  gn = mod / stats / gene
  # , eq = mod / stats / eqvl
)
