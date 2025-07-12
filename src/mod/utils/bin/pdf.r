box::use(
  stats[approxfun, integrate],
)

# kde to probability distribution function
as.pdf <- function(kde, ...) {
  # assert args
  stopifnot(
    "'kde' must be a 'density' object." =
      class(kde) == "density"
  )

  # approximate pdf
  kde |>
    approxfun(
      yleft = 0,
      yright = 0,
      ...
    ) ->
  pdf.approx

  # normalize pdf
  (
    pdf.approx |>
      integrate(-Inf, Inf)
  )[[1]] -> const.norm

  return(
    function(x) {
      pdf.approx(x) / const.norm
    }
  )
}

box::export(as.pdf)
