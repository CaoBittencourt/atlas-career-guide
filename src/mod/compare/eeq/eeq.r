# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  glogis = mod / utils / logistic
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
      # x = 1 + years,
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
  return(
    glogis$logistic(
      x = years,
      a = 0,
      k = 1,
      # k = min(years / min_years, 1),
      c = 1,
      q = 1 / min_years,
      m = min_years,
      # b = 1,
      # b = min_years / years,
      b = min_years,
      nu = 1
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
eeq <- function(years, min_years, eeq_method = eeq.methods$linear_logistic, ...) {
  # assert args
  assert$base$validate.numeric.bounded(years, "years", F, lb = 0)
  assert$base$validate.numeric.bounded(min_years, "min_years", F, lb = 0, lc = F)
  assert$base$validate.method(eeq_method, "eeq_method", eeq.methods)

  # multiple dispatch
  if (eeq_method[[1]] == eeq.methods$binary) {
    return(eeq.binary(years, min_years))
  }

  if (eeq_method[[1]] == eeq.methods$logistic) {
    return(eeq.logistic(years, min_years))
  }

  if (eeq_method[[1]] == eeq.methods$linear) {
    return(eeq.linear(years, min_years))
  }

  if (eeq_method[[1]] == eeq.methods$linear_logistic) {
    return(eeq.linear_logistic(years, min_years))
  }
}

# endregion
# exports
# region: exports
box::export(eeq, eeq.methods)

# endregion
