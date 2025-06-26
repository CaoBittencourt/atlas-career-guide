# setup
# region: modules
modular::project.options('atlas')

# endregion
# region: imports
box::use(
  mod/roadmap/path[paths]
)

# endregion
# region: export
paths$table |> 
  write.csv(
    file.path(
      getOption("atlas.data"),
      'graph.csv'
    )
  )

# endregion