# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use()

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# squared similarity matrix = temp employability matrix
"atlas.root" |>
  getOption() |>
  file.path(
    "models",
    "evol",
    "similarity.rds"
  ) |>
  readRDS() ->
mtx_similarity

mtx_similarity[-c(1, 2)] |> as.matrix() -> employability_mtx
employability_mtx |> rownames() <- mtx_similarity$to

# squared similarity matrix = temp employability matrix
employability_mtx^2 -> employability_mtx


# labor stats
getOption("atlas.labor") |> readRDS() -> df_labor

# max number of job progressions
stages <- 7

# endregion
# model
# region: morph / markov model
prog.morph <- function(employability_mtx, criterion, employment, stages) {
  # assert args in main function

  ncol(employability_mtx) -> n

  # brackets / career progression stages
  findInterval(
    criterion,
    seq(
      criterion |> min(),
      criterion |> max(),
      length.out = stages + 1
    ),
    left.open = T
  ) -> bracket

  # criterion-adjusted transition matrix
  employability_mtx * (
    criterion > (
      criterion |>
        matrix(n, n) |>
        t()
    )
  ) * (
    bracket > (
      bracket |>
        matrix(n, n) |>
        t()
    )
  ) -> transition_mtx

  transition_mtx |> diag() <- 1

  which(
    transition_mtx > 0,
    arr.ind = T
  ) |>
    as_tibble() |>
    select(
      from = col,
      to = row
    ) |>
    mutate(
      prog = as.list(from)
    ) |>
    group_by(from) |>
    mutate(n = n()) |>
    ungroup() |>
    inner_join(
      tibble(
        from = criterion |> order(decreasing = T),
        rank = seq_along(criterion)
      ),
    ) |>
    arrange(rank) ->
  valid.prog

  # which(
  #   transition_mtx > 0,
  #   arr.ind = T
  # ) |>
  #   as_tibble() |>
  #   select(
  #     from = col,
  #     to = row
  #   ) |>
  #   mutate(
  #     prog = as.list(to)
  #   ) ->
  # valid.prog

  # valid.prog |>
  #   group_by(from) |>
  #   tally() |>
  #   inner_join(
  #     tibble(
  #       from = criterion |> order(decreasing = T),
  #       rank = seq_along(criterion)
  #     ),
  #   ) |>
  #   arrange(rank) -> career.rank

  return(list(
    probs = transition_mtx,
    valid = valid.prog,
    # order = career.rank,
    stage = tibble(
      id = seq_along(bracket),
      stage = bracket
    )
  ))
}

# endregion
# region: sketch
employability_mtx |>
  prog.morph(
    df_labor$wage,
    employment = NULL,
    stages = 5
  ) -> dsds
dsds$valid -> valid.prog

i <- 4
for (i in 1:22) {
  valid.prog |>
    filter(rank <= i) |>
    rename_with(
      ~ paste0(.x, ".to")
    ) ->
  valid.prog.to

  valid.prog |>
    inner_join(
      valid.prog.to
      # |>
      # filter(
      #   rank.to == i
      # ),
      ,
      by = c("to" = "from.to"),
      relationship = "many-to-many"
    ) |>
    mutate(
      prog = Map(c, prog, prog.to)
      # ,
      # prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
    ) |>
    select(
      rank, n,
      from, to,
      prog
    ) |>
    bind_rows(
      valid.prog |>
        filter(to != (
          valid.prog |>
            filter(rank == i) |>
            pull(from)
        ))
    ) -> valid.prog
}


# endregion
# region: recursive join
recursive.join <- function(valid.prog, valid.prog.to, iter = 1) {
  print(valid.prog)

  valid.prog |>
    inner_join(
      valid.prog.to |>
        filter(
          rank.to == iter
        ),
      by = c("to" = "from.to")
    ) |>
    mutate(
      prog = Map(c, prog, prog.to)
      # ,
      # prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
    ) |>
    select(
      rank, n,
      from, to,
      prog
    ) |>
    bind_rows(
      valid.prog |>
        filter(to != (
          valid.prog |>
            filter(rank == iter) |>
            pull(from)
        ))
    ) ->
  valid.prog.to

  print(valid.prog)

  if (iter < 873) {
    return(
      valid.prog |>
        recursive.join(
          valid.prog.to,
          iter = iter + 1
        )
    )
  }

  return(valid.prog)
}

# 250 * 8 * 40 == 80000 hours
# i.e. if one stays only 1 year at each job, they can have only up to 40 jobs in their working life
# this, however, is too high of a number of career progressions
# a more reasonable maximum parameter would be no more than 20 unique jobs with 2 years each (for job hoppers)
# for individuals interested in building an actual career, though, ~8 jobs with 5 years on average seems more realistic
# recursive.join <- function(valid.prog, nto.prog, iter = 1) {
#   print(nto.prog)

#   valid.prog |>
#     setNames(
#       valid.prog |>
#         names() |>
#         paste0(".to")
#     ) |>
#     inner_join(
#       nto.prog,
#       by = c("from.to" = "to"),
#       keep = T,
#       relationship = "many-to-many"
#     ) |>
#     mutate(
#       prog = Map(c, prog, prog.to),
#       prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
#     ) |>
#     select(
#       from, to,
#       prog, prog.str, nto
#     ) |>
#     unique() ->
#   nto.prog

#   # nto.prog |>
#   #   setNames(
#   #     nto.prog |>
#   #       names() |>
#   #       paste0(".to")
#   #   ) |>
#   #   inner_join(
#   #     valid.prog,
#   #     by = c("from.to" = "to"),
#   #     keep = T,
#   #     relationship = "many-to-many"
#   #   ) |>
#   #   mutate(
#   #     prog = Map(c, prog, prog.to)
#   #   ) |>
#   #   select(
#   #     from, to,
#   #     prog, nto
#   #   ) -> nto.prog

#   print(nto.prog)

#   if (iter <= 3) {
#     return(
#       valid.prog |>
#         recursive.join(
#           nto.prog,
#           iter = iter + 1
#         )
#     )
#   }

#   return(list(
#     valid = valid.prog,
#     nto = nto.prog
#   ))
# }

# recursive.join <- function(valid.prog, valid.prog.to, iter = 1) {
#   print(valid.prog)

#   valid.prog |>
#     inner_join(
#       valid.prog.to |>
#         filter(
#           rank.to == iter
#         ),
#       by = c("to" = "from.to")
#     ) |>
#     mutate(
#       prog = Map(c, prog, prog.to)
#       # ,
#       # prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
#     ) |>
#     select(
#       rank, n,
#       from, to,
#       prog
#     ) |>
#     bind_rows(
#       valid.prog |>
#         filter(to != (
#           valid.prog |>
#             filter(rank == iter) |>
#             pull(from)
#         ))
#     ) ->
#   valid.prog

#   print(valid.prog)

#   if (iter < 873) {
#     return(
#       valid.prog |>
#         recursive.join(
#           valid.prog.to,
#           iter = iter + 1
#         )
#     )
#   }

#   return(valid.prog)
# }

# endregion
# region: test
employability_mtx |>
  prog.morph(
    df_labor$wage,
    employment = NULL,
    stages = 5
  ) ->
dsds

dsds$valid |>
  recursive.join(
    dsds$valid |>
      rename_with(
        ~ paste0(.x, ".to")
      )
  ) ->
dsdsdsds


dsds$valid
dsds$order

dsds$valid |>
  filter(!(
    from %in% (
      dsds$valid |>
        filter(
          n == 1
        ) |>
        pull(from)
    )
  )) |>
  inner_join(
    dsds$valid |>
      filter(
        n == 1
      ) |>
      rename_with(
        .fn = ~ paste0(.x, ".to"),
      ),
    by = c("to" = "from.to")
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  select(
    from, to,
    prog, prog.str, n
  )

dsds$valid |>
  rename_with(
    .fn = ~ paste0(.x, ".to"),
  ) -> valid.prog.to


dsds$valid |>
  filter(
    to != (dsds$valid |> filter(rank == 1) |> pull(from))
  ) |>
  select(
    from, to,
    prog, n
  ) |>
  bind_rows(
    dsds$valid |>
      # filter(
      #   rank != 1
      # ) |>
      inner_join(
        valid.prog.to |>
          filter(
            rank.to == 1
          ),
        by = c("to" = "from.to")
      ) |>
      mutate(
        prog = Map(c, prog, prog.to),
        prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
      ) |>
      select(
        from, to,
        prog, n
      )
  )



# endregion
# region: sketch
employability_mtx |>
  prog.morph(
    df_labor$wage,
    employment = NULL,
    stages = 5
  ) ->
dsds

dsds$valid
dsds$valid

(dsds$probs > 0) |>
  colSums() |>
  sort()
dsds$valid
dsds$order |>
  inner_join(dsds$stage) |>
  group_by(stage) |>
  group_split()

dsds$valid |>
  mutate(
    prog.str = paste0(from, "=>", to),
    prog = as.list(from)
  ) ->
dsds.prog

dsds.prog |>
  inner_join(
    dsds$order,
    by = c("from" = "id")
  ) |>
  group_by(from) |>
  mutate(
    nto = n()
  ) |>
  ungroup() |>
  arrange(rank) ->
dsds.prog

# dsds.prog |> filter(nto == 2)
dsds.prog |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |> filter(nto == 3),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  select(
    from, to,
    prog, prog.str, nto
  ) |>
  unique()

dsds.prog |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |> filter(nto == 1),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  select(
    from, to,
    prog, prog.str, nto
  ) |>
  unique()

dsds.prog |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |> filter(nto == 6),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  select(
    from, to,
    prog, prog.str, nto
  ) |>
  unique()

dsds.prog |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |> filter(nto == 6),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  select(
    from, to,
    prog, prog.str, nto
  ) |>
  unique()

dsds.prog |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |> filter(rank == 1),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  select(
    from, to,
    prog, prog.str, nto
  ) |>
  unique()

dsds.prog |>
  inner_join(
    dsds$order,
    by = c("from" = "id")
  ) |>
  select(
    from, nto, rank
  ) |>
  unique() |>
  arrange(rank) |>
  mutate(
    dsdsdsdsdds = nto <= rank
  ) |>
  pull(dsdsdsdsdds) |>
  all()
print(n = 1000)

dsds.prog |>
  inner_join(
    dsds$order,
    # by = c("to" = "id"),
    by = c("from" = "id"),
    relationship = "many-to-many"
  ) |>
  arrange(rank) |>
  select(
    from, nto, rank
  ) |>
  unique() |>
  mutate(
    nto.diff = nto - lag(nto, default = first(nto))
  ) |>
  print(n = 1000)

dsds.prog |> filter(nto == 1)
dsds.prog |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |>
      filter(nto == 3),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to),
    prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
  ) |>
  mutate() |>
  select(
    from, to,
    prog, prog.str, nto
  ) |>
  print(n = 100)
#   unique()


dsds.prog

dsds.prog |>
  filter(nto == 1) |>
  setNames(
    dsds.prog |>
      names() |>
      paste0(".to")
  ) |>
  inner_join(
    dsds.prog |>
      filter(nto == 3) |>
      setNames(
        dsds.prog |>
          names() |>
          paste0(".to")
      ) |>
      inner_join(
        dsds.prog |> filter(nto == 3),
        by = c("from.to" = "to"),
        keep = T,
        relationship = "many-to-many"
      ) |>
      mutate(
        prog = Map(c, prog, prog.to)
      ) |>
      select(
        from, to,
        prog, nto
      ) |>
      unique(),
    by = c("from.to" = "to"),
    keep = T,
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = Map(c, prog, prog.to)
  ) |>
  select(
    from, to,
    prog, nto
  ) |>
  unique()


dsds$order$rank |>
  lapply(
    function(rk) {
      print(rk)
      dsds.prog |>
        setNames(
          dsds.prog |>
            names() |>
            paste0(".to")
        ) |>
        inner_join(
          dsds.prog |> filter(rank == rk),
          by = c("from.to" = "to"),
          keep = T,
          relationship = "many-to-many"
        ) |>
        mutate(
          prog = Map(c, prog, prog.to),
          prog.str = Map(paste0, prog, collapse = "=>") |> unlist()
        ) |>
        select(
          from, to,
          prog, prog.str, nto
        )
    }
  ) ->
lalala

lalala[1]
lalala[2]
lalala[3]
lalala[4]
lalala[5]
lalala[6]
lalala[7]
lalala[8]
lalala[9]
lalala[10]
lalala[11]
lalala[12]
lalala[13]
lalala[14]
lalala[15]
lalala[16]
lalala[17]
lalala[18]
lalala[19]
lalala |> bind_rows() -> lalalalala

lalalalala |> unique()

dsds.prog |>
  recursive.join(
    dsds.prog |>
      filter(
        rank == 19
      )
  ) -> lalala

lalala$nto



dsds.prog |>
  filter(from == 27) |>
  pull(to) %in% (lalala$nto |> unique() |> pull(to))

recursive.join <- function(valid.prog, order) {
  # note: the algorithm is the same for both "morph" and "markov" methods,
  # but "markov" uses a constant (static) employability matrix
  # Wk1 ... Wk1
  # ... ... ...
  # Wkn ... Wkn

  if (length(order)) {
    return(
      valid.prog |>
        filter(to == order[1]) |>
        inner_join(
          # left_join(
          valid.prog |>
            filter(
              from == order[1]
            ) |>
            rename(
              prog.to = prog,
              from.to = from,
              to.to = to
            ),
          by = c("to" = "from.to"),
          relationship = "many-to-many"
        ) |>
        mutate(
          prog = paste0(prog, "=>", to.to)
        ) |>
        select(
          -ends_with(".to")
        ) |>
        # full_join(
        #   valid.prog
        # ) |>
        recursive.join(
          order[-1]
        )
    )
  }

  return(valid.prog)
}

dsds$valid |>
  mutate(
    prog = paste0(from, "=>", to)
  ) |>
  recursive.join(dsds$order$id[1]) |>
  bind_rows(
    dsds$valid |>
      mutate(
        prog = paste0(from, "=>", to)
      )
  ) |>
  recursive.join(dsds$order$id[27])

dsds$valid |>
  mutate(
    prog = paste0(from, "=>", to)
  ) ->
dsds.prog

dsds.prog |>
  group_by(from) |>
  mutate(
    nto = n()
  ) |>
  ungroup() |>
  arrange(nto) |>
  print(n = 100)

dsds.prog |> filter(from == 126)

dsds.prog |>
  filter(
    to ==
      dsds$order |>
        arrange(rank) |>
        # arrange(-rank) |>
        slice(1) |>
        pull(id)
  ) |>
  full_join(
    dsds.prog |>
      filter(
        from ==
          dsds$order |>
            arrange(rank) |>
            # arrange(-rank) |>
            slice(1) |>
            pull(id)
      ) |>
      rename(
        prog.to = prog,
        from.to = from,
        to.to = to
      ),
    by = c("to" = "from.to"),
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = paste0(prog, "=>", to.to)
  ) |>
  select(
    -ends_with(".to")
  ) |>
  right_join(
    dsds.prog,
    by = c(
      "from", "to"
    )
  ) |>
  filter(
    is.na(prog.x)
  )

dsds$order |>
  arrange(rank) |>
  slice(1) |>
  inner_join(
    dsds$valid,
    by = c("id" = "from")
  ) |>
  inner_join(
    dsds$valid,
    relationship = "many-to-many"
  )

# endregion
# region: markov career progression model
mtx_similarity[-c(1, 2)] |> as.matrix() -> transition_mtx
transition_mtx |> rownames() <- mtx_similarity$to

# squared similarity matrix = temp employability matrix
transition_mtx^2 -> transition_mtx

# preference-adjusted transition matrix
transition_mtx * (
  df_labor$wage >= (
    df_labor$wage |>
      matrix(873, 873) |>
      t()
  )
) -> transition_mtx

# employment-weighted transition matrix
# transition_mtx * (
#   df_labor$
#     employment_variants /
#     sum(
#       df_labor$
#         employment_variants
#     ) |>
#       matrix(873, 873)
# ) -> transition_mtx

# p(x2|x1) * p(x3|x2)
transition_mtx[1:3, 1:3] -> dsds
transition_mtx[1:5, 1:5] -> dsds

# t(dsds)
dsds
which(dsds > 0, arr.ind = T)
which(dsds > 0, arr.ind = T)[, c(2, 1)] |>
  as_tibble() |>
  left_join(
    which(dsds > 0, arr.ind = T) |>
      as_tibble(),
    by = c("row" = "col"),
    relationship = "many-to-many"
  ) |>
  setNames(
    paste0("t", 1:3)
  )

which(dsds > 0, arr.ind = T) |>
  as_tibble(rownames = "occupation") |>
  relocate(1, 3, 2) |>
  left_join(
    which(dsds > 0, arr.ind = T) |>
      as_tibble(rownames = "occupation") |>
      relocate(1, 3, 2) |>
      rename(
        col = row,
        row = col
      ),
    by = c("col" = "col")
  )


# which(dsds[, 1] > 0)

career.paths <- function(transition_mtx, k = 1, paths = list()
                         # , cache = list()
) {
  # transition_mtx[, k] |> which.max() -> q
  which(transition_mtx[, k] > 0) -> q
  if (any(k == q)) {
    return(c(paths, ))
  }

  return(career.paths(transition_mtx, q[1], paths))
}

dsds |> career.paths()


# accountants' career paths
# Cross product of elements provided to CJ() would result in 257123894257828039270175080448 rows which exceeds .Machine$integer.max == 2147483647
# data.table::CJ(
#   # replicate(2, 1:873, F)
#   1:873,
#   1:873,
#   1:873,
#   1:873,
#   1:873,
#   1:873,
#   1:873,
#   1:873,
#   1:873,
#   1:873
# )
# |>
#  as_tibble() |>
#   mutate(
#     prob = c(
#       "Pr[1|1] * Pr[1|1] * Pr[1|1]",
#       "Pr[1|1] * Pr[1|1] * Pr[2|1]",
#       "Pr[1|1] * Pr[1|1] * Pr[3|1]",

#       "Pr[1|1] * Pr[2|1] * Pr[3|1]"
#     )
#   )

# 111 with prob = Pr[1|1] * Pr[1|1] * Pr[1|1]
# 113 with prob = Pr[1|1] * Pr[1|1] * Pr[3|1]
# 133 with prob = Pr[1|1] * Pr[3|1] * Pr[3|3]
# 333 with prob = Pr[3|1] * Pr[3|3] * Pr[3|3]



# endregion
# region: employability matrix
# squared similarity matrix = temp employability matrix
mtx_similarity[-1] |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "employability"
  ) |>
  mutate(
    employability = employability^2,
    employability = employability * (employability > 0.5)
  ) ->
df_prog

# endregion
# region: progression 1x1 combinations
df_prog |>
  inner_join(
    df_labor |>
      rename(
        employment.to = employment_variants,
        wage.to = wage
      ),
    by = c(
      "to" = "occupation"
    )
  ) |>
  inner_join(
    df_labor |>
      rename(
        employment.from = employment_variants,
        wage.from = wage
      ),
    by = c(
      "from" = "occupation"
    )
  ) |>
  select(
    from, to,
    employability,
    employment.to,
    employment.from,
    wage.to,
    wage.from
  ) ->
df_prog

# endregion
# # region: career stages (wage brackets)
# df_prog |>
#   filter(
#     wage.from <= wage.to
#   ) |>
#   mutate(
#     wage.diff = wage.to - wage.from
#   ) |>
#   group_by(from) |>
#   mutate(
#     stage =
#       findInterval(
#         wage.diff,
#         seq(
#           wage.diff |> min(),
#           wage.diff |> max(),
#           length.out = stages + 1
#         ),
#         left.open = T
#       )
#   ) ->
# df_prog

# # endregion
# region: valid career progressions (yk <= yq, Wkq > 0)
df_prog |>
  filter(
    wage.from <= wage.to
  ) |>
  filter(
    employability > 0
  ) ->
df_prog

# endregion
# region: progression probability and expected payoff
df_prog |>
  group_by(from) |>
  mutate(
    # probability of finding each job at each stage of career progression
    # note 0 <= sum(prob) <= 1 for each from, stage group
    # thus, because 0 <= employability <= 1, probs don't have to sum to 1
    prob = employability * employment.to / sum(employment.to)
    # prob = employability
  ) |>
  ungroup() |>
  select(
    from,
    to,
    employability,
    prob,
    wage = wage.to
  )
group_by(from) |>
  arrange(-employability, -prob, -wage, .by_group = T) |>
  slice(stages) |>
  ungroup() |>
  filter(
    from == first(from)
  )
# |>
#   group_by(stage) |>
#   group_split()





# endregion
# # region: progression probability and expected payoff
# df_prog |>
#   group_by(from, stage) |>
#   mutate(
#     # probability of finding each job at each stage of career progression
#     # note 0 <= sum(prob) <= 1 for each from, stage group
#     # thus, because 0 <= employability <= 1, probs don't have to sum to 1
#     prob = employability * employment.to / sum(employment.to)
#     # prob = employability
#   ) |>
#   ungroup() |>
#   select(
#     stage,
#     from,
#     to,
#     prob,
#     wage.to = wage.to
#   ) |>
#   group_by(from, stage) |>
#   arrange(-prob, -wage.to, .by_group = T) |>
#   slice(1) |>
#   ungroup() |>
#   filter(
#     from == first(from)
#   )
# # |>
# #   group_by(stage) |>
# #   group_split()





# # endregion
# region: dsds
mtx_similarity |>
  select(
    to,
    employability = `Accountants and Auditors`
  ) |>
  mutate(
    employability = employability^4,
    employability = employability * (employability > 0.5)
  ) |>
  inner_join(
    df_labor |>
      select(
        to = occupation,
        employment.to = employment_variants,
        wage.to = wage,
        wage.bracket.to = wage.bracket
      )
  ) |>
  mutate(
    wage.from =
      df_labor |>
        filter(
          occupation == "Accountants and Auditors"
        ) |>
        pull(wage),
    wage.bracket.from =
      df_labor |>
        filter(
          occupation == "Accountants and Auditors"
        ) |>
        pull(wage) |>
        findInterval(
          seq(
            df_labor$wage |> min(),
            df_labor$wage |> max(),
            length.out = 11
          )
        )
  ) |>
  filter(
    wage.from <= wage.to
  ) |>
  filter(
    employability > 0
  ) |>
  group_by(wage.bracket.to) |>
  mutate(
    employment.bracket = sum(employment.to),
    prob = employability * employment.to / sum(employment.to)
  ) |>
  select(
    stage, from, to,
    prob,
    wage = wage.to
  )
arrange(
  -wage.to * prob,
  .by_group = T
) |>
  slice(1) |>
  ungroup() |>
  arrange(
    wage.to
  )

# endregion
# region: career progression candidates (yk <= yq)
df_prog |>
  filter(
    wage.from < wage.to
  ) |>
  arrange(
    from,
    -employability,
    -wage.to
  )

# endregion
# region: career progression brackets (yb = yq)

# endregion
# region: valid career progressions (yk <= yq, Wkq > 0)

# endregion
# region: expected payoff for each career path

# endregion
# region: expected payoff overall

# endregion
