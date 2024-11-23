# region: box modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# modules search path
options("box.path" = file.path(box::file(), "modules"))

# endregion
# region: imports

# endregion
# region: assert skill set
#' @export
assert_skill_set <- function(x) {
  stopifnot(
    '"skill_set" must be a numeric vector."' =
      is.numeric(x)
  )
}

# endregion
# region: assert correlation matrix
assert_correlation <- function(x) {
  stopifnot(
    '"dsds" must be a numeric correlation matrix"' =
      all(
        is.numeric(x),
        is.matrix(x),
        all(x) >= -1,
        all(x) <= 1
      )
  )
}

# endregion
