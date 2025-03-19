# validate arg
validate <- function(x, type, nullable, arg.name, type.def) {
  if (nullable) {
    if (!is.null(x)) {
      if (!type(x)) {
        # stop if invalid type
        stop(paste0("'", arg.name, "'", " must be either NULL or ", type.def))
      }
    }
  } else {
    if (!type(x)) {
      # stop if invalid type
      stop(paste0("'", arg.name, "'", " must be ", type.def))
    }
  }
}
