# region: imports
box::use(
  types = mod / utils / assert / utils / nullable[...]
)

# endregion
# region: validate arg
validate <- function(x, type, nullable, arg.name, type.def) {
  # nullable
  if (nullable) {
    # call type function with nullable function
    if (!types$nullable(type(x))) {
      # stop if invalid type
      stop(paste0("'", arg.name, "'", " must be either NULL or ", type.def))
    }
  }

  # non-nullable
  # call type function
  if (!type(x)) {
    # stop if invalid type
    stop(paste0("'", arg.name, "'", " must be ", type.def))
  }
}

# endregion
# region: exports
box::export(validate)

# endregion
