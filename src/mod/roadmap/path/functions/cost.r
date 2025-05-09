# setup
# region: imports
box::use(
  mod / roadmap / path / functions / restart[...],
)

# endregion
# dispatch
# region: movement cost
move.cost <- function(skq, xk, xq, tk, tq) {
  # assert args in main function
  # equivalent similarity
  # skq.eq <- (skq >= 0.5) * skq
  skq.eq <- ((skq^2) >= 0.5) * skq

  # xp and edu requirements
  pmax((xq - xk * skq.eq), 0) / skq -> req.x
  pmax((tq - tk * skq.eq), 0) / skq -> req.t

  # career move duration in years
  # restart if inefficient carry-over
  return(restart(xq, tq, req.x, req.t))
}

# endregion
# exports
# region: exports
box::export(move.cost)

# endregion
