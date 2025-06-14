# imports
box::use(
  pracma[integral, integral2]
)

# independent probability
prob.x <- function(pdf.x, x.from = -Inf, x.to = Inf) {
  return(integral(pdf.x |> Vectorize(), x.from, x.to)[[1]])
}

prob.x |> Vectorize(c("x.from", "x.to")) -> prob.x

# conditional probability
prob.y_x <- function(pdf.y_x, x.from, x.to, y.from, y.to, ...) {
  return(
    integral2(
      pdf.y_x |> Vectorize(),
      x.from,
      x.to,
      y.from,
      y.to,
      ...
    )[[1]]
  )
}

prob.y_x |> Vectorize(c("x.from", "x.to", "y.from", "y.to")) -> prob.y_x

# conditional probability normalizing constant
norm.const <- function(pdf.y_x, xmin, xmax, ymin, ymax, ...) {
  return(
    2 * integral2(
      pdf.y_x,
      xmin, xmax,
      ymin, ymax
    )[[1]]
  )
}

norm.const |> Vectorize(c("xmin", "xmax", "ymin", "ymax")) -> norm.const

# joint probability
prob.xy <- function(pdf.x, pdf.y_x, x.from, x.to, y.from, y.to, ...) {
  return(
    prob.x(
      pdf.x,
      x.from,
      x.to
    ) * prob.y_x(
      pdf.y_x,
      x.from,
      x.to,
      y.from,
      y.to,
      ...
    )
  )
}

# exports
box::export(
  prob.x,
  prob.y_x,
  norm.const,
  prob.xy
)
