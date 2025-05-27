# setup
# region: imports
box::use(
  assert = mod / utils / assert,
)

# endregion
# methods
# region: pdf method
bin.pdf <- function(x, bins, ...) {
  # assert args in main function
  # description
  return("pdf")
}

# endregion
# region: kde method
bin.kde <- function(x, bins, ...) {
  # assert args in main function
  # description
  return("kde")
}

# endregion
# region: default method
bin.default <- function(x, bins, ...) {
  # assert args in main function
  # description
  return("default")
}

# endregion
# dispatch
# region: bin generic function
bin <- function(x, bins, ...) {
  # assert args

  # multiple dispatch
  if (x |> is.function()) {
    return(bin.pdf(x, bins, ...))
  }

  if (class(x) == "density") {
    return(bin.kde(x, bins, ...))
  }

  return(bin.default(x, bins, ...))
}

# endregion
# exports
# region: exports
box::export(bin, bin.methods)

# endregion
