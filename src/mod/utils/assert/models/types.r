# setup
# region: imports
box::use(
  types = mod / utils / assert / base / types[...],
)

# endregion
# vectors
# region: skill set
# a skill set is a vector in the unit interval
is.skill.set <- function(x) {
  return(types$is.unit(x))
}

# endregion
# region: attribute equivalence
# attribute equivalence is just a scaled skill set (in the unit interval)
is.aeq <- function(x) {
  return(types$is.unit(x))
}

# endregion
# region: skill set generality
# skill set generality is a percentage
is.gene <- function(x) {
  return(types$is.unit(x))
}

# endregion
# region: skill set competence
# skill set competence is a percentage
is.comp <- function(x) {
  return(types$is.unit(x))
}

# endregion
# region: employment levels

# endregion
# region: wages

# endregion
# matrices
# region: skill set matrix
is.skill.set.matrix <- function(x) {
  return(types$is.unit.matrix(x))
}

# endregion
# region: attribute equivalence matrix
is.aeq.matrix <- function(x) {
  return(types$is.unit.matrix(x))
}

# endregion
# exports
# region: exports
box::export(
  is.skill.set,
  is.aeq,
  is.gene,
  is.comp,
  is.skill.set.matrix,
  is.aeq.matrix
)

# endregion
