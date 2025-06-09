modular::project.options("atlas")

box::use(
  dplyr[...],
  tidyr[...],
  rs2 = reshape2,
)

list(
  prog = list(
    analytic = list(
      math = list(
        recursive = NA
      ) # status = value, desc = name
    ),
    dijkstra = list(
      path.mod = list(
        movement.type = list(
          switch = 1,
          work = 1,
          study = 1,
          reset = 1
        ),
        movement.prob = list(
          indie.dist = 1,
          joint.dist = 0.5
        )
      )
    )
  ),
  dsds = list(
    NA
  ),
  lalala = list(
    analytic = list(
      math = list(
        recursive = NA
      ) # status = value, desc = name
    ),
    dijkstra = list(
      path.mod = list(
        movement.type = list(
          switch = 1,
          work = 1,
          study = 1,
          reset = 1
        )
      )
    )
  ),
  prog = list(
    analytic = list(
      math = list(
        recursive = NA
      ) # status = value, desc = name
    )
  ),
  prog = list(
    NA
  )
) -> tasks

count.recursive <- function(x){
  if(!is.list(x)){
    return(seq_along(x))
  }

  return(
    x |> unlist() |> lapply(count.recursive)
  )

}

tasks |> count.recursive()

tasks

recursive.id <- function(x){
  if(!is.list(x)){
    return(x)
  }

  return(
    x |> lapply(recursive.id)
  )
}


tasks$prog$analytic$math$recursive
tasks$prog$dijkstra$path.mod$movement.type$switch
tasks$prog$dijkstra$path.mod$movement.type$work
tasks$prog$dijkstra$path.mod$movement.type$study
tasks$prog$dijkstra$path.mod$movement.type$reset
tasks$prog$dijkstra$path.mod$movement.prob$indie.dist
tasks$prog$dijkstra$path.mod$movement.prob$joint.dist
tasks$dsds
tasks$lalala$analytic$math$recursive
tasks$lalala$dijkstra$path.mod$movement.type$switch
tasks$lalala$dijkstra$path.mod$movement.type$work
tasks$lalala$dijkstra$path.mod$movement.type$study
tasks$lalala$dijkstra$path.mod$movement.type$reset
tasks$prog$analytic$math$recursive
tasks$prog$dijkstra$path.mod$movement.type$switch
tasks$prog$dijkstra$path.mod$movement.type$work
tasks$prog$dijkstra$path.mod$movement.type$study
tasks$prog$dijkstra$path.mod$movement.type$reset
tasks$prog$dijkstra$path.mod$movement.prob$indie.dist
tasks$prog$dijkstra$path.mod$movement.prob$joint.dist

tasks |> rapply()

tasks

na.string <- '<NA>'
tasks |> rs2$melt(value.name = 'status') -> tasks.nested
tasks.nested[,
  tasks.nested |> colnames() |> order()
] -> tasks.nested

tasks.nested

tasks.nested |> 
  mutate(
    projId = as.factor.chr(L1)
  )



tasks.nested |> 
  mutate(
    taskId = row_number()
  ) |> 
  pivot_longer(
    cols = -c(status, taskId),
    names_to = 'level',
    values_to = 'task'
  ) |> 
  mutate(
    level = 
      level |> 
      extract_numeric(),
    task = 
      if_else(
    task |> extract_numeric() |> is.na(),
    task,
    ''
      )
  ) |> 
  filter(
    task != na.string
  ) |>
  group_by(level == 1) |> 
  arrange(level, .by_group = T) |> 
  mutate(
    levelId = 
  )
  group_by(taskId) |> 
  arrange(level, .by_group = T) |> 
  mutate(
    from = lag(task),
    to = task
  )

tasks.df$level |> extract_numeric()

filter(!(level |> is.na()))

tasks |> 
  flatten_dfc() |> 
  pivot_longer(
  cols = everything(),
  names_to = 'task',
  values_to = 'status'
) -> task.values

task.values$task -> task.names

tasks |> reshape2::melt()

tasks |> lapply(names) -> dsds

# Map(
#   function(x, x.lag){

#   },
#   x = tasks |> names(),
# )

tasks |> lapply(names) |> names()
tasks |> nestmap(names)
tasks |> nestmap(nestmap, names)
tasks |> nestmap(nestmap, nestmap, names)
tasks |> nestmap(nestmap, nestmap, nestmap, nestmap, names)
tasks |> nestmap(nestmap, nestmap, nestmap, nestmap, nestmap, names)


names(tasks)[which(names(tasks) %in% task.names)]

task.names


tasks |> lapply(names)


'analytic' %in% names(tasks)
'analytic' %in% names(tasks$prog)

tasks

tasks

tasks$dsds |> names()


task.names |> lapply(function(x) tasks[[x]])
tasks

tasks |> recursive.names() -> dsds

dsds$prog$analytic$math |> class()
dsds$prog$dijkstra$path.mod$movement.type
task.values
task.names
recursive.names <- function(x){
  if(is.list(x[[1]])){
    return(
      x |> lapply(recursive.names)
    )
  }

  return(
    unlist(x)
  )
}

tasks |> 
task.names[1]

tasks

tasks


tasks$prog$analytic$math$recursive

recursive.names <- function(x){
  if(!is.list(x)){
    return(x)
  }

  return(
    x |> bind_rows(.id = 'task')
  )
}

tasks |> recursive.names()

tasks$prog$analytic$math$recursive
tasks |> names() |> unlist() |> names() |> stringr::str_split('\\.') -> tasks.path

tasks.path |> lapply(lag) |> lapply(last, na_rm = T)



tasks |> list_flatten()

tasks |> lapply(function(tasks) tasks |> bind_rows(.id = 'task'))

tasks |> list() |> data.table::rbindlist(fill = T)
tasks |> list() |> purrr::map_dfr(as_tibble)

unlist.df <- function(x){
  if(!is.list(x)){
    return(x)
  }

  return(x |> bind_rows(.id = 'dsds') |> unlist.df())
}

tasks |> unlist.df() -> dsdsds
tasks |> bind_rows(.id = 'dsds')

tasks |> unlist(recursive = F, use.names = T) -> dsds

dsds$