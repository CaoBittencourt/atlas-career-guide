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
      Sys.getenv("ATLAS_DATA"),
      'graph.csv'
    )
  )

# endregion