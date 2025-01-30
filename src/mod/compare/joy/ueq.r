# region: imports
box::use(
  eq = mod / describe / aeq
)

# endregion
# region: utility equivalence function
ueq <- function(pref_set, ...) {
  # assert in aeq function
  # call aeq with args
  return(pref_set |> eq$aeq(...))
}

# endregion
# region: exports
box::export(ueq)

# endregion
