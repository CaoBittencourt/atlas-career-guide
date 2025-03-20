# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  utils = mod / utils / math[cosine]
)

# endregion
# methods
# region: cosine method
field.cosine <- function() {
  # assert args in main function
  # description
  return("cosine")
}

# endregion
# region: cobb_douglas method
field.cobb_douglas <- function() {
  # assert args in main function
  # description
  return("cobb_douglas")
}

# endregion
# region: list of methods
list(
  "cosine" = "cosine",
  "cobb_douglas" = "cobb_douglas"
) -> field.methods

# endregion
# dispatch
# region: field generic function
field <- function(skill_set, skill_matrix, field_method = field.methods[[1]], ...) {
  # assert args

  # multiple dispatch
  if (field_method[[1]] == field.methods$cosine) {
    return(field.cosine())
  }

  if (field_method[[1]] == field.methods$cobb_douglas) {
    return(field.cobb_douglas())
  }
}

# endregion
# exports
# region: exports
box::export(field, field.methods)

# endregion
