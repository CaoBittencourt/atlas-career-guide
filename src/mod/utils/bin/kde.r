box::use(
  stats[density],
  assert = mod / utils / assert,
)

# discrete data to kernel density estimation
as.kde <- function(x, prob, lb = NULL, ub = NULL, n = 1024, ...) {
  # assert args
  lb |> assert$base$validate.numeric("lb", T)
  ub |> assert$base$validate.numeric("ub", T)
  n |> assert$base$validate.numeric.bounded("n", F, 1)
  x |> assert$base$validate.numeric.bounded("x", F, lb, ub)
  prob |> assert$base$validate.numeric.bounded("prob", F, 0, 1)
  stopifnot(
    "'x' and 'prob' must be the same length." =
      length(x) == length(prob)
  )

  # approximate a probability density function from data
  if (all(length(lb), !length(ub))) {
    return(
      density(
        x = x,
        n = n,
        weights = prob,
        from = lb,
        ...
      )
    )
  }

  if (all(!length(lb), length(ub))) {
    return(
      density(
        x = x,
        n = n,
        weights = prob,
        to = ub,
        ...
      )
    )
  }

  if (all(length(lb), length(ub))) {
    return(
      density(
        x = x,
        n = n,
        weights = prob,
        from = lb,
        to = ub,
        ...
      )
    )
  }

  return(
    density(
      x = x,
      n = n,
      weights = prob,
      ...
    )
  )
}


box::export(as.kde)
