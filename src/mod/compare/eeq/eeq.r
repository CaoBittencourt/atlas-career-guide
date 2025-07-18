# setup
# region: imports
box::use(
  assert = utils / assert,
  glogis = utils / logistic
)

# endregion
# methods
# region: binary method
eeq.binary <- function(years, min_years) {
  # assert args in main function
  return(as.numeric(years >= min_years))
}

# endregion
# region: linear method
eeq.linear <- function(years, min_years) {
  # assert args in main function
  return(pmin(years / min_years, 1))
}

# endregion
# region: logistic method
eeq.logistic <- function(years, min_years) {
  # assert args in main function
  return(
    glogis$logistic(
      x = years,
      a = 0,
      k = 1,
      c = 1,
      q = 1,
      m = min_years,
      b = 1,
      nu = 1
    )
  )
}

# endregion
# region: linear_logistic method
eeq.linear_logistic <- function(years, min_years) {
  # assert args in main function

  # apply generalized logistic function with parameters
  x <- pmin(years / min_years, 1)
  m <- 0.5

  return(
    glogis$logistic(
      x = x,
      m = m,
      a = 0,
      k = x,
      c = 1,
      q = m * (1 - x),
      nu = x / (m * (x != 1)),
      b = 1 / (1 - m)
    )
  )
}

# endregion
# region: list of methods
list(
  "binary" = "binary",
  "linear" = "linear",
  "logistic" = "logistic",
  "linear_logistic" = "linear-logistic"
) -> eeq.methods

# endregion
# dispatch
# region: eeq generic function
eeq <- function(
  years,
  min_years,
  similarity,
  eeq_method = eeq.methods$linear_logistic,
  ...
) {
  # assert args
  assert$base$validate.numeric.bounded(similarity, "similarity", F, 0, 1)
  assert$base$validate.numeric.bounded(years, "years", F, lb = 0)
  assert$base$validate.numeric.bounded(
    min_years,
    "min_years",
    F,
    lb = 0,
    lc = F
  )
  assert$base$validate.method(eeq_method, "eeq_method", eeq.methods)

  # multiple dispatch
  if (eeq_method[[1]] == eeq.methods$binary) {
    return(eeq.binary(similarity * years, min_years))
  }

  if (eeq_method[[1]] == eeq.methods$logistic) {
    return(eeq.logistic(similarity * years, min_years))
  }

  if (eeq_method[[1]] == eeq.methods$linear) {
    return(eeq.linear(similarity * years, min_years))
  }

  if (eeq_method[[1]] == eeq.methods$linear_logistic) {
    return(eeq.linear_logistic(similarity * years, min_years))
  }
}

# endregion
# exports
# region: exports
box::export(eeq, eeq.methods)

# endregion
