box::use(
  pa = roadmap / models / default
)

# aliases with default data
path.methods <- pa$path.methods
path.efficiency <- pa$path.efficiency.detailed
path <- pa$path.detailed
vertex.cost <- pa$vertex.cost.detailed
paths <- pa$paths$detailed
path.timeline <- pa$path.timeline.detailed
path.cost <- pa$path.cost.detailed

box::export(
  path.methods,
  path.efficiency,
  path,
  vertex.cost,
  paths,
  path.timeline,
  path.cost
)
