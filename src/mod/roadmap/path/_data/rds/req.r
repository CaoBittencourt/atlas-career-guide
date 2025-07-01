# setup
# region: modular
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  readr[...],
  dplyr[...],
  tidyr[...],
  stringr[...],
  stats[...],
  readxl[read_excel],
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

tmax <- 20

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

xmax <- 60

# endregion
# region: onet data
# ids
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

# endregion
# region: similarity matrix
df_ids |>
  select(
    to = occupation
  ) |>
  inner_join(
    getOption("atlas.root") |>
      file.path(
        "articles",
        "1.introduction-matching",
        "output",
        "similarity_matrix.csv"
      ) |>
      read_csv() |>
      rename(
        to = 1
      ) |>
      relocate(
        df_ids$occupation
      )
  ) |>
  as_tibble() |>
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~.x
      # .fns = ~ .x^2
    )
  ) ->
df_similarity

# endregion
# region: closest matches
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
  group_by(to) |>
  filter(
    id_soc_code.to %in% df_ids$id_soc_code
  ) |>
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
          !(id_soc_code %in% df_ids$id_soc_code)
        ) |>
        pull(id))
    ) |>
    pull(id_soc_code.to) %in%
    df_ids$id_soc_code
)

onet_req |>
  inner_join(
    df_closest_match |>
      select(
        id.replace = id.from,
        id = id.to,
        id_soc_code.replace = id_soc_code.from,
        id_soc_code = id_soc_code.to
      ) |>
      inner_join(
        df_ids
      ) |>
      transmute(
        id = id,
        occupation = occupation,
        id_soc_code = if_else(
          !(
            id_soc_code %in%
              onet_req$id_soc_code
          ),
          id_soc_code.replace,
          id_soc_code
        )
      ),
    relationship = "many-to-many"
  ) |>
  arrange(
    id,
    scaleId,
    years
  ) ->
onet_req

# endregion
# model
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
  ) |>
  mutate(
    to = replace_na(to, tmax)
  ) ->
onet.bin$t

onet.bin$x |>
  inner_join(
    tibble(
      binId = experience |> seq_along(),
      type = experience |> names()
    )
  ) |>
  mutate(
    to = replace_na(to, xmax)
  ) ->
onet.bin$x

# endregion
# region: basic education
basic.education.id <- max(df_ids$id) + 1

df_ids |>
  bind_rows(
    tibble(
      id = basic.education.id,
      occupation = "Basic Education"
    )
  ) ->
df_ids

onet.bin$t |>
  bind_rows(
    tibble(
      id = basic.education.id,
      binId = 1,
      from = 0,
      to = education$high.school,
      pct = 1,
      type = "high.school"
    )
  ) ->
onet.bin$t

onet.bin$x |>
  bind_rows(
    tibble(
      id = basic.education.id,
      binId = 1,
      from = 0,
      to = 0,
      pct = 1,
      type = "intern"
    )
  ) ->
onet.bin$x

# endregion
# region: requirements data frame
onet.bin |>
  bind_rows(
    .id = "var"
  ) |>
  group_by(id) |>
  filter(
    pct > 0
  ) |>
  group_by(id, var) |>
  reframe(
    from = min(from)
  ) |>
  pivot_wider(
    id_cols = id,
    names_from = var,
    values_from = from
  ) |>
  inner_join(
    df_ids
  ) ->
career.req

# endregion
# region: similarity matrix
df_similarity |>
  mutate(
    `Basic Education` = 1
  ) ->
df_similarity

df_similarity |>
  bind_rows(
    c(
      "Basic Education",
      rep(1, 874) |> as.list()
    ) |>
      setNames(
        df_similarity |>
          names()
      ) |>
      as_tibble()
  ) ->
df_similarity

# endregion
# exports
# region: exports
saveRDS(
  df_similarity,
  getOption("atlas.mod") |>
    file.path(
      "roadmap",
      "path",
      "data",
      "rds",
      "similarity.rds"
    )
)

save(
  df_ids,
  education,
  experience,
  career.req,
  onet.bin,
  file =
    getOption("atlas.mod") |>
      file.path(
        "roadmap",
        "path",
        "data",
        "rds",
        "req.rdata"
      )
)

# endregion
