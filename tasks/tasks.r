# setup
# modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# imports
box::use(
  tt = utils / misc / tasks,
  st = stringr,
  stats[setNames],
)

# data
getOption("atlas.mod") |>
  list.files(
    recursive = T,
    full.names = T
  ) -> task.files

task.files[
  task.files |>
    st$str_ends(".tasks")
] -> task.files

task.files |>
  lapply(tt$read.tasks) |>
  setNames(
    task.files |>
      st$str_remove_all(
        ".tasks"
      ) |>
      basename()
  ) -> tasks

# plot
tasks |>
  lapply(tt$plot.tasks) -> tasks.plots

tasks.plots$roadmap
