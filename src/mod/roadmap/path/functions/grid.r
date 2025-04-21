# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / utils / data[sublist],
  req = mod / roadmap / path / data / req,
)

# endregion
# dispatch
# region: career grid
career.grid <- function(xmin, tmin, xmax = NULL, tmax = req$education$doctorate) {
  # assert args in main function
  # generate valid career progressions on a 2d experience vs education grid
  return(
    expand.grid(
      x = req$experience |> sublist(function(x) ifelse(!length(xmax), x >= xmin, x >= xmin & x <= xmax)) |> as.numeric(),
      t = req$education |> sublist(function(t) (t >= tmin) & (t <= tmax)) |> as.numeric()
    )
  )
}

# endregion
# exports
# region: exports
box::export(career.grid)

# endregion
