box::use(
  pa = roadmap / models / default
)

# aliases with default data
path.methods <- pa$path.methods
path.efficiency <- pa$path.efficiency.expected
path <- pa$path.expected
vertex.cost <- pa$vertex.cost.expected
paths <- pa$paths$expected
path.timeline <- pa$path.timeline.expected
path.cost <- pa$path.cost.expected

box::export(
  path.methods,
  path.efficiency,
  path,
  vertex.cost,
  paths,
  path.timeline,
  path.cost
)
