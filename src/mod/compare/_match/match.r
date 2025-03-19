# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# region: method1 method
match.method1 <- function() {
  # assert args in main function
  # description
  return("method1")
}

# endregion
# region: method2 method
match.method2 <- function() {
  # assert args in main function
  # description
  return("method2")
}

# endregion
# region: list of methods
list(
  "method1" = "method1",
  "method2" = "method2"
) -> match.methods

# endregion
# region: match generic function
match <- function(match_method = match.methods[[1]], ...) {
  # assert args

  # multiple dispatch
  if (match_method[[1]] == match.methods$method1) {
    return(match.method1())
  }

  if (match_method[[1]] == match.methods$method2) {
    return(match.method2())
  }
}

# endregion
# region: exports
box::export(match, match.methods)

# endregion
