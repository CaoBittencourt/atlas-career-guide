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
prog.morph <- function(employability_mtx, criterion, employment = NULL) {
  # assert args in main function

  ncol(employability_mtx) -> n

  # criterion-adjusted transition matrix
  employability_mtx * (
    # criterion >= (
    criterion > (
      criterion |>
        matrix(n, n) |>
        t()
    )
  ) -> transition_mtx

  transition_mtx |> diag() <- 1

  return(list(
    probs = transition_mtx,
    valid = which(transition_mtx > 0, arr.ind = T) |> as_tibble() |> select(from = col, to = row),
    order = tibble(
      id = criterion |> order(decreasing = T),
      rank = seq_along(criterion)
    )
  ))
}

# endregion
# region: test
employability_mtx |>
  prog.morph(df_labor$wage) ->
dsds

recursive.join <- function(valid.prog, order) {
  if (length(order)) {
    return(
      valid.prog |>
        filter(to == order[1]) |>
        inner_join(
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
  recursive.join(dsds$order$id[1]) ->
dsdsds

dsds$valid |>
  mutate(
    prog = paste0(from, "=>", to)
  ) |>
  filter(to == dsds$order$id[1]) |>
  inner_join(
    dsds$valid |>
      filter(from == dsds$order$id[1]) |>
      rename(
        from.to = from,
        to.to = to
      ),
    # suffix = c("", ".to"),
    by = c("to" = "from.to"),
    relationship = "many-to-many"
  ) |>
  mutate(
    prog = paste0(prog, "=>", to.to)
  ) |>
  select(-to.to)


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
# # region: dsds
# mtx_similarity |>
#   select(
#     to,
#     employability = `Accountants and Auditors`
#   ) |>
#   mutate(
#     employability = employability^4,
#     employability = employability * (employability > 0.5)
#   ) |>
#   inner_join(
#     df_labor |>
#       select(
#         to = occupation,
#         employment.to = employment_variants,
#         wage.to = wage,
#         wage.bracket.to = wage.bracket
#       )
#   ) |>
#   mutate(
#     wage.from =
#       df_labor |>
#         filter(
#           occupation == "Accountants and Auditors"
#         ) |>
#         pull(wage),
#     wage.bracket.from =
#       df_labor |>
#         filter(
#           occupation == "Accountants and Auditors"
#         ) |>
#         pull(wage) |>
#         findInterval(
#           seq(
#             df_labor$wage |> min(),
#             df_labor$wage |> max(),
#             length.out = 11
#           )
#         )
#   ) |>
#   filter(
#     wage.from <= wage.to
#   ) |>
#   filter(
#     employability > 0
#   ) |>
#   group_by(wage.bracket.to) |>
#   mutate(
#     employment.bracket = sum(employment.to),
#     prob = employability * employment.to / sum(employment.to)
#   ) |>
#   select(
#     stage, from, to,
#     prob,
#     wage = wage.to
#   )
# arrange(
#   -wage.to * prob,
#   .by_group = T
# ) |>
#   slice(1) |>
#   ungroup() |>
#   arrange(
#     wage.to
#   )

# # endregion
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
