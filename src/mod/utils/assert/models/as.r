# setup
# region: imports
box::use(
  types = mod / utils / assert / base / as[...],
)

# endregion
# matrices
# region: skill set matrix
as.skill.set.matrix <- function(x, arg.name = NULL) {
  return(as.data.frame(types$as.unit.matrix(x, arg.name)))
}

# endregion
# region: attribute equivalence matrix
as.aeq.matrix <- function(x, arg.name = NULL) {
  return(as.data.frame(types$as.unit.matrix(x, arg.name)))
}

# endregion
# exports
# region: exports
box::export(
  as.skill.set.matrix,
  as.aeq.matrix
)

# endregion
