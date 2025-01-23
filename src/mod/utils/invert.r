# finds the inverse of a function
box::use(stats[uniroot])

invert <- function(fn, lb, ub, ...) {
  return(
    Vectorize(
      function(y) {
        uniroot(
          function(x) {
            fn(x) - y
          },
          lower = lower,
          upper = upper,
          ...
        )$root
      }
    )
  )
}
