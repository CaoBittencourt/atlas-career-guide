# validate arg
validate <- function(x, type, nullable, arg.name = NULL, type.def) {
  if (nullable) {
    if (!(length(x) == 0)) {
      # if (!is.null(x)) {
      if (!type(x)) {
        # stop if invalid type
        if (!length(arg.name)) {
          stop(paste0("Argument must be either NULL or ", type.def))
        }

        stop(paste0("'", arg.name, "'", " must be either NULL or ", type.def))
      }
    }
  } else {
    if (!type(x)) {
      # stop if invalid type
      if (!length(arg.name)) {
        stop(paste0("Argument must be ", type.def))
      }

      stop(paste0("'", arg.name, "'", " must be ", type.def))
    }
  }
}
