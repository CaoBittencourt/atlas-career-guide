modular::project.options("atlas")

box::use(
  dplyr[...],
  tidyr[...],
  str = stringr,
  ig = igraph,
  # tidygraph[...],
  vctrs[new_data_frame],
)

# library(ggraph)
# library(tidygraph)

read.tasks <- function(path) {
  path |>
    readLines(warn = F) |>
    str$str_trim("right") ->
  tasks

  tasks[tasks != ""] -> tasks

  tasks |> str$str_count("  ") -> indentation
  indentation / ifelse(all((indentation %% 2) == 0), 2, 1) -> indentation
  indentation |> as.integer() -> indentation

  data.frame(
    title = tasks |> str$str_trim(),
    task = indentation |> seq_along(),
    nest = indentation
  ) ->
  tasks.lines

  tasks.lines$title |> str$str_split(":", 2, T) -> tasks

  tasks[, 1] |> str$str_trim() -> tasks.lines$title
  tasks[, 2] |> str$str_trim() -> tasks.lines$status
  tasks.lines$status[tasks.lines$status == ""] <- "0"
  tasks.lines$status |> as.numeric() -> tasks.lines$status

  tasks.lines$isProj <- tasks.lines$nest == 0
  tasks.lines$proj <- cumsum(tasks.lines$isProj)

  mapply(
    function(n, l) {
      max(
        tasks.lines$task[
          (
            tasks.lines$nest + ifelse(
              tasks.lines$task < n, 0, Inf
            )
          )
          < l
        ]
      ) -> id

      id[is.infinite(id)] <- n

      return(id)
    },
    n = tasks.lines$task,
    l = tasks.lines$nest
  ) |>
    as.numeric() ->
  tasks.lines$from

  tasks.lines[
    c(
      "title",
      "proj",
      "task",
      "from",
      "nest",
      "status"
    )
  ] ->
  tasks.lines

  return(
    tasks.lines |>
      new_data_frame(
        class = c(
          class(tasks.lines),
          "tasks"
        )
      )
  )
}

graph.tasks <- function(tasks) {
  stopifnot(
    "'tasks' must be a data frame with the 'tasks' subclass." = all(
      is.data.frame(tasks),
      any("tasks" %in% class(tasks))
    )
  )

  return(
    tasks |> relocate(task, from) |>
      ig$graph_from_data_frame() |>
      ig$set.edge.attribute(
        "status",
        value = tasks$status
      )
  )
}

"/home/Cao/storage/github/atlas/src/mod/utils/misc/dsds.tasks" |> read.tasks() -> tasks

# tasks |>
#   group_by(from) |>
#   reframe(
#     dsds = mean(status, na.rm = T)
#   ) |>
#   right_join(
#     tasks,
#     by = c(
#       "from" = "task"
#     ),
#   )
library(ggraph)
library(tidygraph)

tasks |>
  graph.tasks() |>
  as_tbl_graph() |>
  ggraph() +
  geom_edge_link(
    aes(
      edge_linetype = tasks$nest
    ),
    alpha = 0.5
  ) +
  geom_node_point(
    aes(
      fill = tasks$status,
      size = max(tasks$nest) - tasks$nest,
      color = tasks$nest |> factor(),
    ),
    stroke = 4,
    shape = 21,
  ) +
  scale_size(
    range = c(20, 40)
  ) +
  geom_node_text(
    aes(
      label = tasks$title |> str$str_wrap(10)
    )
  ) +
  scale_edge_linetype_binned() +
  scale_color_manual(
    values = c("purple", "darkblue", rep("white", tasks$nest |> unique() |> length() - 2)) |> setNames(tasks$nest |> unique()),
  ) +
  scale_fill_gradient(
    low = "lightgrey",
    high = "green",
    na.value = "red"
  ) +
  labs(
    title = "Task Roadmap",
    subtitle = "Current status as of" |> paste(Sys.Date() |> format(format = "%m-%d-%Y")),
  ) +
  guides(
    fill = F,
    color = F,
    shape = F,
    stroke = F,
    size = F,
    edge_linetype = F
  ) +
  theme_graph(
    title_size = 40,
    subtitle_size = 20,
  )
