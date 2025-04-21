# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  req = mod / roadmap / path / data / req,
)

# endregion
# dispatch
# region: movement cost
move.cost <- function(skq, xk, xq, tk, tq) {
  # # remove baseline education
  # tk <- tk - req$education$high.school
  # tq <- tq - req$education$high.school

  # equivalent similarity
  skq.eq <- ((skq^2) >= 0.5) * skq
  # skq.eq <- (skq >= 0.5) * skq

  # xp and edu requirements
  (xq - xk * skq.eq) -> req.x
  (tq - tk * skq.eq) -> req.t

  (req.x > 0) * req.x -> req.x
  (req.t > 0) * req.t -> req.t

  # allow for educational restart (via education "occupation")
  # for the first levels of education
  # e.g. start new major from scratch
  # carry-over education and experience: req.t and req.x
  # carry-over experience, restart education: tq - restart and req.x
  # full restart: tq - restart and xq
  ifelse(
    xq == req$education$associate & req.t > req$restart$associate,
    req$restart$associate,
    req.t
  ) ->
  req.t

  ifelse(
    xq == req$education$bachelor & req.t > req$restart$bachelor,
    req$restart$bachelor,
    req.t
  ) ->
  req.t

  # career move duration in years
  return((req.x + req.t) / skq)
}

# endregion
# exports
# region: exports
box::export(move.cost)

# endregion
