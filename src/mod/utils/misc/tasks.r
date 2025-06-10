modular::project.options("atlas")

box::use(
  dplyr[...],
  tidyr[...],
  str = stringr,
  ig = igraph,
)

parse.tasks <- function(path) {
  path |>
    readLines(warn = F) |>
    str$str_trim("right") ->
  tasks

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
    tasks.lines
  )
}


parse.tasks("/home/Cao/storage/github/atlas/src/mod/utils/misc/dsds.txt")
