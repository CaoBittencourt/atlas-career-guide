box::use(
  stats[approxfun],
)

# kde to probability distribution function
as.pdf <- function(kde, ...) {
  # assert args
  stopifnot(
    "'kde' must be a 'density' object." =
      class(kde) == "density"
  )

  # normalize pdf
  # const.norm <- integrate(approx.pdf,-Inf,Inf)
  # return(
  #   function(x) {
  #     approx.pdf(x) / const.norm

  #   }
  # )
  return(
    kde |>
      approxfun(
        yleft = 0,
        yright = 0,
        ...
      )
  )
}
