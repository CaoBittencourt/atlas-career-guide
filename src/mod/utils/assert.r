# region: imports

# endregion
# region: assert skill set
valid_skill_set <- function(x) {
  stopifnot(
    '"skill_set" must be a numeric vector in the unit interval."' = all(
      is.numeric(x), x <= 1, x >= 0
    )
  )
}

# endregion
# region: exports
box::export(
  valid_skill_set
)

# endregion
