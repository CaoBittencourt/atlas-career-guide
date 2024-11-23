# region: assert skill set
#' @export
valid_skill_set <- function(x) {
  stopifnot(
    '"skill_set" must be a numeric vector."' =
      is.numeric(x)
  )
}

# endregion