# region: imports

# endregion
# region: assert skill set
valid_skill_set <- function(skill_set) {
  stopifnot(
    '"skill_set" must be a numeric vector in the unit interval."' = all(
      is.numeric(skill_set),
      skill_set >= 0,
      skill_set <= 1
    )
  )
}

# endregion
# region: assert skill set matrix
valid_skill_mtx <- function(skill_mtx) {
  stopifnot(
    '"skill_mtx" must be a numeric matrix or data frame in the unit interval."' = all(
      any(is.matrix(skill_mtx), is.data.frame(skill_mtx)),
      skill_mtx >= 0,
      skill_mtx <= 1
    )
  )
}

# endregion
# region: as skill set matrix
as.skill_mtx <- function(skill_mtx) {
  # skill sets as columns (for lapply)
  cbind(skill_mtx) -> skill_mtx
  # rbind(skill_mtx) -> skill_mtx
  valid_skill_mtx(skill_mtx)
  # rownames(skill_mtx) <- NULL
  return(as.data.frame(skill_mtx))
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
  valid_skill_mtx,
  as.skill_mtx,
  valid_generality
)

# endregion
