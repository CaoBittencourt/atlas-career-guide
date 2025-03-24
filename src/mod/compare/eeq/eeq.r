modular::project.options("atlas")
# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# methods
# region: binary method
eeq.binary <- function() {
  # assert args in main function
  # description
  return("binary")
}

# endregion
# region: linear method
eeq.linear <- function() {
  # assert args in main function
  # description
  return("linear")
}

# endregion
# region: logistic method
eeq.logistic <- function() {
  # assert args in main function
  # description
  return("logistic")
}

# endregion
# region: linear_logistic method
eeq.linear_logistic <- function() {
  # assert args in main function
  # description
  return("linear_logistic")
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
  assert$base$validate.numeric



  # multiple dispatch
  if (eeq_method[[1]] == eeq.methods$binary) {
    return(eeq.binary())
  }

  if (eeq_method[[1]] == eeq.methods$logistic) {
    return(eeq.logistic())
  }
}

# endregion
# exports
# region: exports
box::export(eeq, eeq.methods)

# endregion
