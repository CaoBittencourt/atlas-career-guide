# region: imports
box::use(
  glogis = mod / utils / logistic
)

# endregion
# region: binary method
eeq.bin <- function(years, min_years) {
  # assert args in main function
  return(as.numeric(years >= min_years))
}

# endregion
# region: linear method
eeq.lin <- function(years, min_years) {
  # assert args in main function
  return(pmin(years / min_years, 1))
}

# endregion
# region: logistic method
eeq.log <- function(years, min_years) {
  # assert args in main function
  return(
    glogis$logistic(
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
# region: generic function
# midpoint, scale parameter?
eeq <- function(years, min_years, eeq_method = c("logistic", "binary", "linear")[[1]]) {
  # assert args
  stopifnot(
    "'years' must be a non-negative number." = all(
      years |> is.numeric(),
      years >= 0
    )
  )
  stopifnot(
    "'min_years' must be a non-negative numeric vector." = all(
      min_years |> is.numeric(),
      min_years >= 0
    )
  )

  stopifnot(
    "'eeq_method' must be one of the following methods: 'logistic', 'binary', 'linear'." = any(
      eeq_method == c("logistic", "binary", "linear")
    )
  )

  # edge-cases

  # multiple dispatch
  eeq_method[[1]] |>
    as.character() |>
    switch(
      "logistic" = return(eeq.log(years, min_years)),
      "binary" = return(eeq.bin(years, min_years)),
      "linear" = return(eeq.lin(years, min_years))
    )
}

# endregion
# region: exports
box::export(eeq)

# endregion
