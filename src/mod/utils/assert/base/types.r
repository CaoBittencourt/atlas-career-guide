# generic types
# region: proper list
is.proper.list <- function(x) {
  return(all(is.list(x), !is.data.frame(x)))
}

# endregion
# region: boolean
is.bool <- function(x) {
  return(all(
    is.logical(x),
    !is.na(x)
  ))
}

# endregion
# numeric types
# region: basic numeric
is.numeric.vector <- function(x) {
  return(x |> vapply(is.numeric, logical(1)) |> all())
}

# endregion
# region: unit
is.unit <- function(x) {
  return(all(
    is.numeric(x),
    x >= 0, x <= 1
  ))
}

# endregion
# region: signed unit
is.unit.signed <- function(x) {
  return(all(
    is.numeric(x),
    x >= -1, x <= 1
  ))
}

# endregion
# region: bernoulli
is.bernoulli <- function(x) {
  return(any(x == 0, x == 1))
}

# endregion
# region: bounded numeric
is.numeric.bounded <- function(x, lb = NULL, ub = NULL, lc = T, rc = T) {
  return(
    all(
      is.numeric.vector(x),
      ifelse(length(lb), ifelse(lc, all(x >= lb), all(x > lb)), T),
      ifelse(length(ub), ifelse(rc, all(x <= ub), all(x < ub)), T)
    )
  )
}

# endregion
# numeric matrix types
# region: unit matrix
is.unit.matrix <- function(x) {
  return(all(
    is.matrix(x),
    is.unit(x)
  ))
}

# endregion
# region: unit signed matrix
is.unit.signed.matrix <- function(x) {
  return(all(
    is.matrix(x),
    is.unit.signed(x)
  ))
}

# endregion
# region: bernoulli matrix
is.bernoulli.matrix <- function(x) {
  return(all(
    is.matrix(x),
    is.bernoulli(x)
  ))
}

# endregion
# region: matrix-like
is.matrix.like <- function(x) {
  return(as.logical(length(dim(x))))
}

# endregion
# region: square matrix
is.square <- function(x) {
  return(nrow(x) == ncol(x))
}

# endregion
