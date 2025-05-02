# setup
# region: imports
# endregion
# dispatch
# region: movement cost of restarting
restart <- function(xq, tq, req.x, req.t) {
  # assert args in main function
  # cost of movement when restarting
  return(pmin(xq, req.x) + pmin(tq, req.t))
}

# endregion
# exports
# region: exports
box::export(restart)

# endregion
