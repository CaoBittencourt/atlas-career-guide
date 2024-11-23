# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# modules search path
options("box.path" = file.path(box::file(), "modules"))

# endregion
# region: imports
# box::use(assert = dir / file)

# endregion
# region: function
#' @export
dsds <- function() {

}

# endregion
