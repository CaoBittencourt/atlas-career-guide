# region: imports

# endregion
# region: is.error
is.error <- function(expr) {
  try(expr) |> class() == "try-error"
}

# endregion
# region: run tests
tests.run <- function(tests, simplify = T) {
  # assert args
  stopifnot(
    "'tests' must be a list of test functions." = all(
      is.list(tests),
      tests |> sapply(class) == "function"
    )
  )

  stopifnot(
    "'simplify' must be either TRUE or FALSE." = all(
      is.logical(simplify),
      !is.na(simplify)
    )
  )

  # call test functions
  tests |> lapply(do.call, args = list()) -> results

  # simplify results
  if (simplify) {
    results |>
      lapply(unlist) |>
      lapply(all) ->
    results
  }

  return(results)
}

# endregion
# region: exports
box::export(
  is.error,
  tests.run
)

# endregion
