# numeric types
# region: unit type
is.unit <- function(x) {
  return(
    all(
      is.numeric(x),
      x >= 0,
      x <= 1
    )
  )
}

# endregion
# region: signed unit type
is.unit.signed <- function(x) {
  return(
    all(
      is.numeric(x),
      x >= -1,
      x <= 1
    )
  )
}

# endregion
# numeric matrix types
# region: unit matrix type
is.unit.matrix <- function(x) {
  return(
    all(
      is.matrix(x),
      is.unit(x)
    )
  )
}

# endregion
# region: unit signed matrix type
is.unit.signed.matrix <- function(x) {
  return(
    all(
      is.matrix(x),
      is.unit.signed(x)
    )
  )
}

# endregion
