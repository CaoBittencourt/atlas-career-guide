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
  return(glogis$sigmoid(years / min_years))
}

# endregion
# region: linear-logistic method
eeq.lnl <- function(years, min_years) {
  # assert args in main function
  return(
    glogis$logistic(
      x = 1 + years,
      a = 0,
      k = 1,
      c = 1,
      q = 1,
      m = min_years,
      # b = min_years / (1 - midpoint), #setup mechanism to avoid NaN
      b = min_years,
      # b = min_years * midpoint,
      nu = 1
    )
  )
}

# endregion
# region: generic function
# midpoint, scale parameter?
eeq <- function(years, min_years, eeq_method = c("linear-logistic", "logistic", "linear", "binary")[[1]]) {
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
    "'eeq_method' must be one of the following methods: 'linear-logistic', 'logistic', 'linear', 'binary'." = any(
      eeq_method == c("linear-logistic", "logistic", "linear", "binary")
    )
  )

  # edge-cases

  # multiple dispatch
  eeq_method[[1]] |>
    as.character() |>
    switch(
      "linear-logistic" = return(eeq.lnl(years, min_years)),
      "logistic" = return(eeq.log(years, min_years)),
      "linear" = return(eeq.lin(years, min_years)),
      "binary" = return(eeq.bin(years, min_years))
    )
}

# endregion
# region: exports
box::export(eeq)

# endregion
