# setup
# region: imports
# endregion
# dispatch
# region: movement cost of restarting
restart <- function(xq, tq, req.x, req.t) {
  # cost of movement when restarting
  return(pmin(xq, req.x) + pmin(tq, req.t))
}

# endregion
# exports
# region: exports
box::export(restart)

# endregion
