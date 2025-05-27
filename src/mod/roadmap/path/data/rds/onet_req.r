# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  readr[...],
  dplyr[...],
  tidyr[...],
  stringr[...],
  stats[...],
  km = flexclust,
  readxl[read_excel],
  bda = bda,
  mod / utils / bin[...],
)

dplyr::filter -> filter

# endregion
# data
# region: required education
list(
  high.school = 17 - 17,
  associate = 19 - 17,
  bachelor = 21 - 17,
  master = 23 - 17,
  doctorate = 28 - 17
) -> education

# endregion
# region: required experience
list(
  intern = 0,
  junior = 1,
  # junior = 2,
  associate = 3,
  mid.level = 5,
  senior = 10
) -> experience

# endregion
# region: onet data
# similarity matrix
getOption("atlas.mod") |>
  file.path(
    "roadmap",
    "path",
    "data",
    "rds",
    "similarity.rds"
  ) |>
  readRDS() ->
df_similarity

# occupations' ids and soc codes
df_similarity$to |>
  as_tibble() |>
  rename(
    occupation = 1
  ) |>
  mutate(
    .before = 1,
    id = row_number()
  ) |>
  inner_join(
    getOption("atlas.oldata") |>
      read.csv() |>
      select(
        id_soc_code,
        occupation
      ) |>
      arrange(
        occupation
      ) |>
      mutate(
        .before = 1,
        id = row_number()
      )
  ) |>
  relocate(
    -occupation
  ) ->
df_ids

# requirements
getOption("atlas.data") |>
  file.path(
    "db_27_3_excel",
    "Education, Training, and Experience.xlsx"
  ) |>
  read_excel() |>
  dplyr::filter(
    `Scale ID` %in% c("RW", "RL")
  ) ->
df_req

# # (Category, `O*NET-SOC Code`, `Scale ID`) are the composite primary key
# all(df_req |> group_by(Category, `O*NET-SOC Code`, `Scale ID`) |> tally() |> pull(n) == 1)
getOption("atlas.data") |>
  file.path(
    "db_27_3_excel",
    "Education, Training, and Experience Categories.xlsx"
  ) |>
  read_excel() |>
  dplyr::filter(
    `Scale ID` %in% c("RW", "RL")
  ) ->
df_req_cat

tribble(
  ~`Scale ID`, ~`Category`, ~`Category Description`, ~`years`,
  "RL", 1, "Less than a High School Diploma", education$high.school,
  "RL", 2, "High School Diploma - or the equivalent (for example, GED)", education$high.school,
  "RL", 3, "Post-Secondary Certificate - awarded for training completed after high school (for example, in agriculture or natural resources, computer services, personal or culinary services, engineering technologies, healthcare, construction trades, mechanic and repair technologies, or precision production)", education$associate,
  "RL", 4, "Some College Courses", education$associate,
  "RL", 5, "Associate's Degree (or other 2-year degree)", education$associate,
  "RL", 6, "Bachelor's Degree", education$bachelor,
  "RL", 7, "Post-Baccalaureate Certificate - awarded for completion of an organized program of study; designed for people who have completed a Baccalaureate degree but do not meet the requirements of academic degrees carrying the title of Master.", education$master,
  "RL", 8, "Master's Degree", education$master,
  "RL", 9, "Post-Master's Certificate - awarded for completion of an organized program of study; designed for people who have completed a Master's degree but do not meet the requirements of academic degrees at the doctoral level.", education$doctorate,
  "RL", 10, "First Professional Degree - awarded for completion of a program that: requires at least 2 years of college work before entrance into the program, includes a total of at least 6 academic years of work to complete, and provides all remaining academic requirements to begin practice in a profession.", education$doctorate,
  "RL", 11, "Doctoral Degree", education$doctorate,
  "RL", 12, "Post-Doctoral Training", education$doctorate,
  "RW", 1, "None", experience$intern,
  "RW", 2, "Up to and including 1 month", 1 / 12,
  "RW", 3, "Over 1 month, up to and including 3 months", mean(c(1, 3)) / 12,
  "RW", 4, "Over 3 months, up to and including 6 months", mean(c(3, 6)) / 12,
  "RW", 5, "Over 6 months, up to and including 1 year", mean(c(6, 12)) / 12,
  "RW", 6, "Over 1 year, up to and including 2 years", mean(c(1, 2)),
  "RW", 7, "Over 2 years, up to and including 4 years", mean(c(2, 4)),
  "RW", 8, "Over 4 years, up to and including 6 years", mean(c(4, 6)),
  "RW", 9, "Over 6 years, up to and including 8 years", mean(c(6, 8)),
  "RW", 10, "Over 8 years, up to and including 10 years", mean(c(8, 10)),
  "RW", 11, "Over 10 years", mean(c(10, 30)),
) ->
df_req_cat

# endregion
# model
# region: data wrangling
df_req |>
  mutate(
    .before = 1,
    id_soc_code =
      `O*NET-SOC Code` |>
        str_sub(1, -4)
  ) |>
  mutate(
    pct = `Data Value` / 100
  ) |>
  inner_join(
    df_req_cat,
    relationship = "many-to-many"
  ) |>
  select(
    scaleId = `Scale ID`,
    id_soc_code,
    onet_soc_code = `O*NET-SOC Code`,
    pct,
    years
  ) ->
onet_req

# get closest match
df_similarity |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".to")),
    by = c(
      "to" = "occupation.to"
    )
  ) |>
  filter(
    id_soc_code.to %in% onet_req$id_soc_code
  ) |>
  group_by(from) |>
  arrange(
    -similarity,
    .by_group = T
  ) |>
  slice(-1) |>
  slice(1) |>
  ungroup() |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".from")),
    by = c(
      "from" = "occupation.from"
    )
  ) ->
df_closest_match

all(
  df_closest_match |>
    filter(
      id.from %in% (df_ids |>
        filter(
          !(id_soc_code %in% onet_req$id_soc_code)
        ) |>
        pull(id))
    ) |>
    pull(id_soc_code.to) %in%
    onet_req$id_soc_code
)

df_closest_match |>
  select(
    id.from,
    id.to,
    id_soc_code.from,
    id_soc_code.to
  ) |>
  inner_join(
    df_ids,
    by = c(
      "id.from" = "id"
    )
  ) |>
  transmute(
    id = id.from,
    occupation = occupation,
    id_soc_code = if_else(
      id_soc_code.from %in%
        onet_req$id_soc_code,
      id_soc_code.from,
      id_soc_code.to
    )
  ) ->
df_ids

onet_req |>
  inner_join(
    df_ids,
    relationship = "many-to-many"
  ) |>
  arrange(
    id,
    scaleId,
    years
  ) ->
onet_req

# endregion
# region: onet binned data => standard numeric requirement bins
Map(
  function(df, bins) {
    df |>
      group_by(id) |>
      reframe(
        years |>
          bin(
            bins,
            pct
          )
      )
  },
  df =
    onet_req |>
      group_by(scaleId) |>
      group_split(),
  bins = list(
    as.numeric(education),
    as.numeric(experience)
  )
) -> onet.bin

onet.bin |> names() <- c("t", "x")

onet.bin$t |>
  inner_join(
    tibble(
      binId = education |> seq_along(),
      type = education |> names()
    )
  ) ->
onet.bin$t

onet.bin$x |>
  inner_join(
    tibble(
      binId = experience |> seq_along(),
      type = experience |> names()
    )
  ) ->
onet.bin$x

# endregion
# region: kde approximation
onet_req |>
  group_by(
    scaleId,
    id
  ) |>
  reframe(
    years = years |> as.kde(pct, 0, max(years)) |> list()
  ) |>
  pivot_wider(
    id_cols = id,
    names_from = scaleId,
    values_from = years
  ) |>
  rename(
    t = RL,
    x = RW
  ) ->
df_kde

# df_kde |>
#   slice(1) |>
#   pull(x) |>
#   purrr::pluck(1) |>
#   plot(
#     xlim = c(-10, 30)
#   )

# df_kde |>
#   slice(1) |>
#   pull(x) |>
#   purrr::pluck(1) |>
#   as.pdf() |>
#   plot(
#     xlim = c(0, 50)
#   )

# onet_req |>
#   filter(
#     id == 1
#   ) |>
#   filter(
#     scaleId == "RW"
#   ) ->
# dsds

# dsds$years |> as.kde(dsds$pct)

# df_kde |>
#   slice(1) |>
#   pull(x) |>
#   purrr::pluck(1)


# endregion
# region: kde => standard numeric requirement bins
df_kde |>
  group_by(id) |>
  reframe(
    x = x |> lapply(bin, bins = as.numeric(experience)) |> lapply(mutate, .after = 1, type = names(experience)),
    t = t |> lapply(bin, bins = as.numeric(education)) |> lapply(mutate, .after = 1, type = names(education)),
  ) ->
df_grid

# df_grid$x |>
#   sapply(function(df) df$x) |>
#   c() |>
#   summary()

# df_grid$t |>
#   sapply(function(df) df$t) |>
#   c() |>
#   summary()

# sum((df_grid$x |> sapply(function(df) df$x) |> c()) == 0)
# sum((df_grid$t |> sapply(function(df) df$t) |> c()) == 0)

# df_pdf |>
#   slice(1) |>
#   pull(x) |>
#   purrr::pluck(1) ->
# dsds

# df_grid |>
#   slice(1) |>
#   pull(x)

# df_grid |>
#   slice(1) |>
#   pull(t)

# df_grid$x |> bind_rows() -> x
# all(x$x >= 0)
# x$x |> ggplot2::qplot(geom = "density", weight = x$pct)

# df_grid$t |> bind_rows() -> t
# all(t$t >= 0)
# t$t |> ggplot2::qplot(geom = "density", weight = t$pct)

# endregion
# region: kde bins vs onet bins
df_grid |>
  select(-t) |>
  unnest(x) |>
  rename(
    pct.kde = pct
  ) |>
  full_join(
    onet.bin$x
  ) |>
  group_by(binId) |>
  reframe(
    `mean(kde - onet)` = mean(pct.kde - pct)
  )

# endregion

# # region: pdf approximation
# df_kde |>
#   group_by(id) |>
#   reframe(
#     x = x |> lapply(as.pdf),
#     t = t |> lapply(as.pdf)
#   ) ->
# df_pdf

# # df_pdf |>
# #   slice(1) |>
# #   pull(x) |>
# #   purrr::pluck(1) |>
# #   plot(
# #     xlim = c(-10, 30)
# #   )

# # df_pdf |>
# #   slice(1) |>
# #   pull(x) |>
# #   purrr::pluck(1) |>
# #   plot(
# #     xlim = c(0, 50)
# #   )

# # endregion
# # region: kde => education-experience requirement types
# # new coefficients: experience vs education relative importance
# # note: compare with all careers and weigh by employment levels
# #                | high experience      | low experience      |
# # high education | rocket science       | education-intensive |
# # low education  | experience-intensive | entry level         |

# # endregion
# # region: kde => kmeans numeric requirement bins
# df_kde |>
#   group_by(id) |>
#   reframe(
#     x = x |> lapply(kmeans.kde, k = length(experience)) |> lapply(as.grid, types = names(experience)) |> lapply(rename, x = 2),
#     t = t |> lapply(kmeans.kde, k = length(education)) |> lapply(as.grid, types = names(education)) |> lapply(rename, t = 2)
#   ) ->
# df_grid

# # df_grid$x |>
# #   sapply(function(df) df$x) |>
# #   c() |>
# #   summary()

# # df_grid$t |>
# #   sapply(function(df) df$t) |>
# #   c() |>
# #   summary()

# # sum((df_grid$x |> sapply(function(df) df$x) |> c()) == 0)
# # sum((df_grid$t |> sapply(function(df) df$t) |> c()) == 0)

# # df_pdf |>
# #   slice(1) |>
# #   pull(x) |>
# #   purrr::pluck(1) ->
# # dsds

# # df_grid |>
# #   slice(1) |>
# #   pull(x)

# # df_grid |>
# #   slice(1) |>
# #   pull(t)

# # df_grid$x |> bind_rows() -> x
# # all(x$x >= 0)
# # x$x |> ggplot2::qplot(geom = "density", weight = x$pct)

# # df_grid$t |> bind_rows() -> t
# # all(t$t >= 0)
# # t$t |> ggplot2::qplot(geom = "density", weight = t$pct)

# # endregion
