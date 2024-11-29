# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# set modules path
options(box.path = file.path(getwd(), "src"))

#' @export
box::use(
  submod1 = relative / path / to / functions1,
  submod2 = relative / path / to / functions2
)
