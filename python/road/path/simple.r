# modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# imports

# optimize cost and career path transition
# assuming morphing hypothesis
# relax morphing with mcts later
move.cost <- function(ßkq, xk, xq, tk, tq) {
  # assert args in main function
  # equivalent similarity
  ßkq.eq <- ßkq * (ßkq >= 0.5)

  # experience gap
  # education gap
  pmax(xq - xk * ßkq.eq, 0) / ßkq -> req.x
  pmax(tq - tk * ßkq.eq, 0) / ßkq -> req.t

  # assume one must have all equivalent years
  # before attempting to switch careers
  # assume order of study and work doesn't matter
  # assume reset cost is zero
  # movement types:
  # - restart x, recycle t
  # - restart t, recycle x
  # - restart t, restart x
  # - recycle t, recycle x
  pmin(xq, req.x) -> req.x
  pmin(tq, req.t) -> req.t

  return(list(
    work = req.x,
    study = req.t,
    total = req.x + req.t
  ))
}

move.cost(0.4, 3, 5, 4, 4)
move.cost(0.8, 3, 5, 4, 4)
move.cost(0, 3, 5, 4, 4)
move.cost(0, 1000, 5, 1000, 4)
