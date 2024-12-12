# region: imports

# endregion
# region: is.error
is.error <- function(expr) {
  try(expr) |> class() == "try-error"
}

# endregion
# region: exports
box::export(
  is.error
)

# endregion
