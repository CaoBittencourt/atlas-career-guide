# setup
# region: imports
box::use(
  assert = utils / assert,
  utils / bin / interval[...],
  dplyr[...],
)

# endregion
# methods
# region: make bins helper
bins_ <- function(x, nbins) {

}

# endregion
# region: bin method
bin.bin <- function(x, pct, bins, ...) {
  # assert args in main function
  # binned data from binned data
  return(
    data.frame(
      binId = x |> findInterval(bins),
      pct = pct / sum(pct)
    ) |>
      inner_join(
        bins |> interval(...)
      ) |>
      group_by(binId) |>
      reframe(
        from = first(from),
        to = first(to),
        pct = sum(pct)
      )
  )
}

# endregion
# region: pdf method
bin.pdf <- function(pdf, bins, ...) {
  # assert args in main function
  # binned data from probability density function
  return("pdf")
}

# endregion
# region: kde standard method
bin.kde <- function(kde, bins, ...) {
  # assert args in main function
  # binned data from kernel density estimation
  return(
    data.frame(
      binId = kde$x |> findInterval(bins),
      pct = kde$y / sum(kde$y)
    ) |>
      inner_join(
        bins |> interval()
      ) |>
      group_by(binId) |>
      reframe(
        from = first(from),
        to = first(to),
        pct = sum(pct)
      )
  )
}

# endregion
# region: kde local minima method

# endregion
# region: kde kmeans method

# endregion
# region: default method
bin.default <- function(x, bins, ...) {
  # assert args in main function
  # binned data from regular data
  return("default")
}

# endregion
# dispatch
# region: bin generic function
bin <- function(x, bins, pct = NULL, ...) {
  # assert args

  # bins
  bins |> as.numeric() -> bins

  # multiple dispatch
  if (x |> is.function()) {
    return(bin.pdf(x, bins, ...))
  }

  if (class(x) == "density") {
    return(bin.kde(x, bins, ...))
  }

  if (list(x, pct) |> vapply(is.numeric, FUN.VALUE = logical(1)) |> all()) {
    return(bin.bin(x, pct, bins, ...))
  }

  return(bin.default(x, bins, ...))
}

# endregion
# exports
# region: exports
box::export(bin)

# endregion
