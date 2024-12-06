# region: imports

# endregion
# region: assert skill set
valid_skill_set <- function(skill_set) {
  stopifnot(
    '"skill_set" must be a numeric vector in the unit interval."' = all(
      is.numeric(skill_set),
      skill_set <= 1,
      skill_set >= 0
    )
  )
}

# endregion
# region: assert generality
valid_generality <- function(generality) {
  stopifnot(
    "'generality' must be either NULL or a number in the unit interval." = any(
      all(
        is.numeric(generality),
        length(generality) == 1,
        generality >= 0,
        generality <= 1
      ),
      is.null(generality)
    )
  )
}
# endregion
# region: exports
box::export(
  valid_skill_set,
  valid_generality
)

# endregion
