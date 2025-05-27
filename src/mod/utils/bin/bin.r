# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / utils / bin / interval[...],
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
      id = x |> findInterval(bins),
      pct = pct
    ) |>
      inner_join(
        bins |> interval()
      ) |>
      group_by(id) |>
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
# region: kde method
bin.kde <- function(kde, bins, ...) {
  # assert args in main function
  # binned data from kernel density estimation
  return(
    data.frame(
      id = kde$x |> findInterval(bins),
      pct = kde$y / sum(kde$y)
    ) |>
      inner_join(
        bins |> interval()
      ) |>
      group_by(id) |>
      reframe(
        from = first(from),
        to = first(to),
        pct = sum(pct)
      )
  )
}

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
box::export(bin)

# endregion
