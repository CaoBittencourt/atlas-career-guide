# setup
# region: imports
box::use(
  models = utils / assert / models / types[...],
  types = utils / assert / base[...],
)

# endregion
# vectors
# region: skill set
# a skill set is a vector in the unit interval
validate.skill.set <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit(x, arg.name, nullable))
}

# endregion
# region: attribute equivalence
# attribute equivalence is just a scaled skill set (in the unit interval)
validate.aeq <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit(x, arg.name, nullable))
}

# endregion
# region: skill set generality
# skill set generality is a percentage
validate.gene <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit(x, arg.name, nullable))
}

# endregion
# region: skill set competence
# skill set competence is a percentage
validate.comp <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit(x, arg.name, nullable))
}

# endregion
# region: attribute macroflexibility
validate.macroflex <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit(x))
}

# endregion
# region: attribute microflexibility
validate.microflex <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit(x))
}

# endregion
# region: employment levels
validate.employment <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.numeric.bounded(x, arg.name, nullable, 0, lc = F))
}

# endregion
# region: wages
validate.wage <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.numeric.bounded(x, arg.name, nullable, 0, lc = F))
}

# endregion
# matrices
# region: skill set matrix
# a skill set matrix is a vector in the unit interval
validate.skill.set.matrix <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit.matrix(x, arg.name, nullable))
}

# endregion
# region: attribute equivalence matrix
# a attribute equivalence matrix is a vector in the unit interval
validate.aeq.matrix <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit.matrix(x, arg.name, nullable))
}

# endregion
# region: attribute microflexibility
validate.microflex.matrix <- function(x, arg.name = NULL, nullable = F) {
  return(types$validate.unit.matrix(x))
}

# endregion
# exports
# region: exports
box::export(
  validate.skill.set,
  validate.aeq,
  validate.gene,
  validate.comp,
  validate.macroflex,
  validate.microflex,
  validate.employment,
  validate.wage,
  validate.skill.set.matrix,
  validate.aeq.matrix,
  validate.microflex.matrix
)

# endregion
