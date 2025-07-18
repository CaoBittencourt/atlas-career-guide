# setup
# region: imports
box::use(
  assert = utils / assert,
  eee = compare / eeq,
)

# endregion
# methods
eee$eeq.methods -> xeq.methods

# endregion
# dispatch
# region: xeq generic function
xeq <- function(
  years,
  min_years,
  similarity,
  xeq_method = xeq.methods$linear_logistic,
  ...
) {
  assert$base$validate.method(xeq_method, "xeq_method", xeq.methods)

  # assert other args in education equivalence function
  # multiple dispatch in education equivalence function
  return(
    eee$eeq(
      years,
      min_years,
      similarity,
      xeq_method = xeq.methods$linear_logistic,
      ...
    )
  )
}

# endregion
# exports
# region: exports
box::export(xeq, xeq.methods)

# endregion
